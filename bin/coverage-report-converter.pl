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

sub report_raw_rows ( $report_file )
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

sub compact_rows ( @raw_rows )
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

my $reports_root = shift or die ( "No value for reports_root" ) ;
my $module_path  = shift or die ( "No value for module_path" ) ;

printf ( "Processing ${module_path}...\n" ) ;

my @raw_rows     = report_raw_rows ( "${reports_root}/${module_path}" ) ;
my @compact_rows = compact_rows ( @raw_rows ) ;

open ( my $fh, ">${reports_root}${module_path}.txt" ) or die ( "open(): $!\n" ) ;
printf $fh ( "mode: set\n" ) ;
foreach my $row ( @compact_rows )
{
  printf $fh (
    "${module_path}:%d.%d,%d.%d 1 %d\n",
    $row->{ line_no_start },
    $row->{ column_no_start },
    $row->{ line_no_end },
    $row->{ column_no_end },
    $row->{ covered }
  ) ;
}
close ( $fh ) ;
