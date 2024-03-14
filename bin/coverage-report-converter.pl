#!/usr/bin/perl
# -*- mode: cperl; cperl-indent-level: 2; -*-
use strict ;
use warnings ;
use diagnostics ;
use utf8 ;
use feature ':5.38' ;
use Encode         qw(decode_utf8) ;
use File::Basename qw(fileparse) ;
use File::Copy     qw(move) ;
use constant {
  true  => 0,
  false => 1
} ;

####################################################################################################

sub read_coverage_data ( $filepath )
{
  open ( my $fh, "<$filepath" ) or die ( "open(): $!\n" ) ;
  foreach ( 0 .. 3 )
  {
    <$fh> ;
  }

  my @rows = () ;
  while ( my $line = <$fh> )
  {
    chomp ( $line ) ;
    $line = decode_utf8 ( $line ) ;
    if ( $line =~ /^\s*\W*\s*$/ )
    {
      next ;
    }
    my @fields = split ( /\s+/, $line ) ;
    my $row =
    {
        file             => $fields[ 0 ],
        clauses          => $fields[ 2 ],
        coverage_percent => $fields[ 3 ],
        failure_percent  => $fields[ 4 ]
    } ;
    push ( @rows, $row ) ;
  }
  return @rows ;
}

####################################################################################################

sub produce_coverage_report ( $coverage_rows, $coverage_file )
{
  my ( $etude_name, $coverage_dir, $ext ) = fileparse ( $coverage_file, ( ".txt", ".cov" ) ) ;
  move ( $coverage_file, "${coverage_file}.bak" ) or die ( "move(): $!\n" ) ;

  open ( my $fh, ">$coverage_file" ) or die ( "open(): $!\n" ) ;

  say $fh ( "-" x 80 ) ;
  say $fh ( "Directory: ${etude_name}" ) ;
  say $fh ( "File\tLines\tExec\tCover\tMissing" ) ;

  my ( $total_clauses, $total_tested ) = ( 0, 0 ) ;
  foreach my $row ( @$coverage_rows )
  {
    my ( $module_name, $module_dir, $ext ) = fileparse ( $row->{ file }, ( ".prolog" ) ) ;
    my $not_covered = ( ( 100 - $row->{ coverage_percent } ) * $row->{ clauses } ) / 100 ;
    my $covered     = $row->{ clauses } - $not_covered ;
    $total_clauses += $row->{ clauses } ;
    $total_tested  += $covered ;
    printf $fh (
      "%s\t%d\t%d\t%.2f\t%d\n", "${module_name}${ext}",     $row->{ clauses },
      $covered,                 $row->{ coverage_percent }, $not_covered
    ) ;
  }
  my $total_coverage    = ( $total_tested / $total_clauses ) * 100 ;
  my $total_not_covered = $total_clauses - $total_tested ;

  printf $fh (
    "TOTAL\t%d\t%d\t%.2f\t%d\n", $total_clauses, $total_tested,
    $total_coverage,             $total_not_covered
  ) ;

  close ( $fh ) ;
}

####################################################################################################

my $coverage_file = shift or die ( "No coverage report specified.\n" ) ;
my @coverage_rows = read_coverage_data ( $coverage_file ) ;
produce_coverage_report ( \@coverage_rows, $coverage_file ) ;
