#!/usr/bin/perl
# -*- mode: cperl; cperl-indent-level: 2; -*-
use strict ;
use warnings ;
use diagnostics ;
use utf8 ;
use feature ':5.38' ;
use Encode         qw(decode_utf8) ;
use File::Basename qw(fileparse) ;
use constant {
  true  => 0,
  false => 1
} ;

####################################################################################################

sub load_prolog_report ( $report_file )
{
  open ( my $fh, "<${report_file}" ) or die ( "open(): $!\n" ) ;
  my @rows     = () ;
  my $in_tests = false ;
  while ( my $line = <$fh> )
  {
    chomp ( $line ) ;
    if ( $line =~ /^\s*(?<line_no>\d+)(?<rest>.*)/ )
    {
      my $line_no = $+{ line_no } ;
      if ( $+{ rest } =~ /^\s*$/ or $+{ rest } =~ /^%/ )
      {
        next ;
      }
      elsif ( $+{ rest } =~ /(\s*(?<coverage>[#+\-\d]+))?(?<text>\s*.*)/ )
      {
        if ( $+{ text } =~ /^\s*%+/ )
        {
          next ;
        }
        elsif ( $+{ text } =~ /^\s*:-\s+begin_tests/ )
        {
          $in_tests = true ;
          next ;
        }
        elsif ( $+{ text } =~ /^\s*:-\s+end_tests/ )
        {
          $in_tests = false ;
          next ;
        }
        elsif ( $+{ text } =~ /^\s*:-/ )
        {
          next ;
        }
        elsif ( $in_tests == true )
        {
          next ;
        }
        else
        {
          my $row =
          {
              line_no  => $line_no,
              coverage => ( $+{ coverage } or "" ),
              text     => $+{ text }
          } ;
          push ( @rows, $row ) ;
        }
      }
      else
      {
        next ;
      }
    }

  }
  return @rows ;
}

####################################################################################################

sub rows_to_clauses ( @raw_rows )
{
  my @clauses   = () ;
  my $in_clause = false ;
  my $clause    = {} ;
  foreach my $raw_row ( @raw_rows )
  {
    if ( $in_clause == true )
    {
      if ( $raw_row->{ text } =~ /\.(\s*%.*)?$/ )
      {
        $clause->{ line_no_end }   = $raw_row->{ line_no } ;
        $clause->{ column_no_end } = length ( $raw_row->{ text } ) ;
        push ( @clauses, $clause ) ;
        $clause    = {} ;
        $in_clause = false ;
      }
    }
    else
    {
      if ( $raw_row->{ text } =~ /^\s*(?<clause_name>\w+).+\.(\s*%.*)?$/ )
      {
        $clause->{ line_no_start }   = $raw_row->{ line_no } ;
        $clause->{ line_no_end }     = $raw_row->{ line_no } ;
        $clause->{ column_no_start } = 1 ;
        $clause->{ column_no_end }   = length ( $raw_row->{ text } ) ;
        if ( $raw_row->{ coverage } =~ /#/ )
        {
          $clause->{ covered } = 0 ;
        }
        else
        {
          $clause->{ covered } = 1 ;
        }
        push ( @clauses, $clause ) ;
        $clause    = {} ;
        $in_clause = false ;
      }
      else
      {
        $clause->{ line_no_start }   = $raw_row->{ line_no } ;
        $clause->{ column_no_start } = 1 ;
        if ( $raw_row->{ coverage } =~ /#/ )
        {
          $clause->{ covered } = 0 ;
        }
        else
        {
          $clause->{ covered } = 1 ;
        }
        $in_clause = true ;
      }
    }
  }
  return @clauses ;
}

####################################################################################################

my $reports_root  = shift or die ( "No value for reports_root" ) ;
my $module_dir    = shift or die ( "No value for module_path" ) ;
my $module_name   = shift or die ( "No value for module_name" ) ;
my $target_report = "${reports_root}${module_dir}coverage.txt" ;

printf ( "Processing ${module_dir}${module_name}.prolog.cov...\n" ) ;

my @raw_rows = load_prolog_report ( "${reports_root}${module_dir}${module_name}.prolog.cov" ) ;
my @clauses  = rows_to_clauses ( @raw_rows ) ;

open ( my $fh, ">>${target_report}" ) or die ( "open(): $!\n" ) ;
if ( -z $target_report )
{
  printf $fh ( "mode: set\n" ) ;
}
foreach my $clause ( @clauses )
{
  printf $fh (
    "${module_dir}${module_name}.prolog:%d.%d,%d.%d 1 %d\n",
    $clause->{ line_no_start },
    $clause->{ column_no_start },
    $clause->{ line_no_end },
    $clause->{ column_no_end },
    $clause->{ covered }
  ) ;
}
close ( $fh ) ;
