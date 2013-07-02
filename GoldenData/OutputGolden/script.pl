#!/usr/bin/perl
use File::Copy

mkdir("development") || print "Unable to create directory <$!>\n";
mkdir("heldout") || print "Unable to create directory <$!>\n";
mkdir("test") || print "Unable to create directory <$!>\n";

my $devCount = 500;
my $heldCount = 500;

my $rangeType = 3     ;

my @files = [];
my $i = 0;
opendir (DIR, ".") or die $!;
while (my $file = readdir(DIR)) {

    if ($file =~ /[^\.]*\.anx/) { 
    @files[$i] = $file;
    $i++;
    }    ;

}  ;

my @devFiles ;
my @heldFiles ;
my @testFiles ;

for $_ (@files) {
 my $type = int rand($rangeType);
 if ($type == 0 && $devCount > 0)
 {
  push @devFiles, $_;
  $devCount--;
  next;
 }
 elsif ($type == 1 && $heldCount > 0)
 {
  push @heldFiles , $_;
  $heldCount--;
  next;
 }
 else 
 {
  push @testFiles, $_;
  next;
 } 
}


 print scalar(@files) . "\n";
 print scalar(@devFiles)  . "\n" ; 
 print scalar(@heldFiles)  . "\n" ; 
 print scalar(@testFiles)  . "\n" ; 
 print scalar(@restFiles)  . "\n" ;
 
 for (@devFiles) {
  copy($_, "./development/".$_);
 }
 print "dev copied" ;
for (@heldFiles) {
  copy($_, "./heldout/".$_);
 } 
print "held copied" ; 
for (@testFiles) {
  copy($_, "./test/".$_);
 }  
print "test copied" ;