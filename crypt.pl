#!/usr/bin/perl

print "Enter a string to encrypt with DES:n";
chomp(my $string = <STDIN>); #Take the input from the user and remove the n

print "Enter two random alphanumerics to be used as a salt:n";
chomp(my $salt = <STDIN>);

my $encrypted_string = crypt($string,$salt); #take the string and the salt and put through crypt()

print qq~
"$string" encrypted using the perl crypt() function and salt "$salt" returns:
$encrypted_string
~;

system("PAUSE");
