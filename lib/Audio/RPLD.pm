#      Copyright (C) Philipp 'ph3-der-loewe' Schafft - 2009-2011
#
#  This file is part of Audio::RPLD,
#  a library to access the RoarAudio PlayList Daemon from Perl.
#  See README for details.
#
#  This file is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 3
#  as published by the Free Software Foundation.
#
#  Audio::RPLD is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this software; see the file COPYING.gplv3.
#  If not, write to the Free Software Foundation, 51 Franklin Street,
#  Fifth Floor, Boston, MA 02110-1301, USA.
#

package Audio::RPLD;

use strict;
use vars qw($VERSION @ISA);

$VERSION     = 0.003;
@ISA         = qw();

#use IO::Socket::UNIX;

=pod

=head1 NAME

Audio::RPLD - Module to communicate with RoarAudio PlayList Daemon

=head1 SYNOPSIS

 use Audio::RPLD;
 
 # Open new connection
 my $rpld = Audio::RPLD->new([$addr[, $type[, $port]]]);
 
 # Work with the connection
 $rpld->play();
 $rpld->stop();
 #...
 
 # close the connection
 $rpld->disconnect();
 
 # reconnect:
 $rpld->connect($addr[, $type[, $port]]);

=head1 DESCRIPTION

This module is used to communicate with a RoarAudio PlayList Daemon (rpld).
It includes support for nearly all of the protocol.

=cut

=pod

=head1 METHODS

=head2 Common Arguments

Here is a list of all the arguments used in this documentation so I do not need to explain later for each method again.

=over

=item $addr

The Address of the server. This can be a filename or host or nodename.
If the type is known to you, you should set the $type argument.

Do not limit the user to hostname or filenames or something. The user should be abled to enter any server location in a freeform text input.

=item $type

This is the type of the address of the server. Currently defined types are:

=over 8

=item UNIX

Use a UNIX socket to connect to the server.

=item DECnet

Use a DECnet socket to connect to the server (currently not supported because there is no IO::Socket::DECnet yet).

=item INET

Use a INET (IPv4) socket to connect to the server.

=item #SOCKET

Use a already connected socket as connection to the server. This can for example be a object opened
with a SSL/TLS or proxy module or any other bidirectional IO object. Object is passed
as $addr parameter.

=item a value of undef

Use Autodetection.

=back

=item $port

If using a INET socket this is the port number to connect to.
Use undef (not include in arguments list) to use defaults.

=item $playlist, $playlist_from, $playlist_to

A Playlist to use. This can be a integer to use the playlist ID (which is preferred), a string to use the playlist name or $any (return value of $rpld->any()) to search thru all playlists and use the first hint. $any is not supported by all commands as it does not make sense for all commands.

=item $vol

A volume. If this is an integer the volume is in range 0 to 65535 (recommended). If it's a string and suffixed by a '%' it is in range 0 to 100. A value of zero always means total silence.

=item $likeness

A floating point describing how much the user likes a song.

=item $name

A string of a name of some object.

=item $ple

A Playlist entry (item) identifier. This is a string of one of the several types. Here is a quick overview:

=over 8

=item Long Global Track Number

The long GTN is a hex string (with leading 0x) prefixed by 'long:' with a length of 64 bit (16 raw digits).
It is used to identify a playlist entry within the runtime of the server process. This one is the one you normally use for all operations like queuing a track for playback.

=item Short Global Track Number

The short GTN is a shorter (only 32 bit) version of the GTN prefixed with 'short:'. It's use is exactly the same as for the long GTN. It exists to get the GTN passed thru things which do not support more than 32 bit. As this is normally not the case for Perl (you pass them around as strings anyway) you should not need to use it at all. It's not recommended to use this one.

=item UUID

The UUID is a string prefixed with 'uuid:' followed by the normal hex-dash notation (example: uuid:54ab0c5f-b058-4c9b-ab3b-dea11b608482). It is used to identify a song. In contrast to the GTN this is not changed by commands such like copy or move operations. The normal use for this is if you want to store something over multiple runtimes of the server process like favorites or something.

If a song does not include a UUID in it's meta data the server will generate a random one so each song has one.

Those UUIDs can also be used together with the Tantalos protocol.

=item Pointer

A pointer is a string (the pointer name) prefixed by 'pointer:' (example: pointer:default). Those are used to have symbolic pointers a playlist entry. The following pointers are currently known by rpld:

=over 8

=item default

If the Main Queue runs out of songs and the current pointer reached the end of the current playlist the current pointer is set to this value and song lookup is done again. For details about the current pointer see below.

=item current

If the Main Queue run out of queued songs the song this pointer points to is automatically queued and this pointer is set to the next entry the the playlist. This is used for 'start here and just play the playlist' behavior. If this pointer reaches the end of the playlist it becomes undefined and if the default pointer is set redefined with it's value. See above for more information about the default pointer.

=item startup

If the startup pointer is set the song it points to is added once at startup of the daemon. This can for example be used to implement boot sounds.

=item temp

The temp pointer is a pointer for user defined jobs. It is the only pointer which is not global (shared with all clients) but a client local pointer. The application can use it for whatever it wants to use it for.

=back

=item Numerical Index

This is a numerical index of the entry. It's format is num:N where N is the index starting with zero. For example num:0 is the first entry in playlist, num:15 is the 16th entry.

=item Likeness Index

This is like the normal numerical index just uses the likeness values of the entrys. This is hardly of use to the user and mainly for internal use. Syntax is likeness:F with F the floating point index.

=item Random Entry

A Random entry can be selected by using random:[PLI]. PLI is a optional parameter. It musst be the ID of the playlist to select entry from. If no playlist is given the current one is used.

=item Random liked Entry

This is like normal random entry but prioritized by the value set with LIKE and DISLIKE commands. Syntax is: randomlike:[PLI].

=back

=item $pointer

A Pointer. This is a string of the pointer name. It is not prefixed with 'pointer:' as when used as PLE.

=back

=head2 Common return values

The following return types are used by methods in this module:

=over

=item $rpld

The instance of this module as returned by new().

=item $res

A general return value. This is undef in case of failure or a defined value in case of no error.
The value may be a string, hash- or arrayref depending on the method. See description of the method for details.

=item $any

The return value of any(). This can be used as wildcard for playlist names in some cases.

=back

=cut

#-------------
# Elementary communication functions

=pod

=head2 Basic functions

=cut

sub new {
 #        0: Socket, 1: State
 my $r = [undef,     0];

 # States: 0 command mode, 1 data recv mode, 2 data transmit mode

 bless($r);

 if ( defined($_[1]) ) {
  return undef unless $r->connect(@_[1..$#_]);
 }

 return $r;
}

=pod

=head3 $rpld = Audio::RPLD-E<gt>new([$addr[, $type[, $port]]])

This method creates a new Audio::RPLD object. If arguments are passed they are directly passed to a call to the connect method (see below) in order to connect to the server. If this fails undef is returned. If no arguments are given you need to connect the object to the server via the connect method later on your own.

=cut

sub connect {
 my ($e, $addr, $type, $port) = @_;
 my $sock;

 if ( (!defined($addr) || $addr eq '') && (!defined($type) || $type eq '') && (!defined($port) || $port eq '') ) {
  return $e->connect_default();
 }

 unless ( $type ) {
  if ( ref($addr) ne '' ) {
   $type = '#SOCKET';
  } elsif ( $addr =~ m#/# ) {
   $type = 'UNIX';
  } elsif ( $addr =~ m#::# ) {
   $type = 'DECnet';
  } else {
   $type = 'INET';
  }
 }

 $type = uc($type);

 if ( $type eq '#SOCKET' ) {
  $sock = $addr;
 } elsif ( $type eq 'UNIX' ) {
  require IO::Socket::UNIX;
  import IO::Socket::UNIX;
  $sock = IO::Socket::UNIX->new($addr);
 } elsif ( $type eq 'INET' ) {
  require IO::Socket::INET;
  import IO::Socket::INET;
  $sock = IO::Socket::INET->new('PeerAddr' => $addr, 'PeerPort' => ($port || 'rpld(24148)'));
 } else {
  return undef;
 }

 return undef unless $sock;

 $e->[0] = $sock;

 return $e;
}

=pod

=head3 $res = $rpld-E<gt>connect($addr[, $type[, $port]])

Connect to a server. You must not call this on a already connected object.

=cut

sub connect_default {
 my ($e) = @_;
 my $home = $ENV{'HOME'} || $ENV{'HOMEDRIVE'}.$ENV{'HOMEPATH'} || '/NXHOMEDIR';
 my @locations = ($home.'/.rpld', qw(/tmp/.rpld /tmp/rpld /var/run/rpld.sock .rpld localhost ::rpld));
 my $server;

 if ( defined($e->[0]) ) {
  return undef;
 }

 $server = $ENV{'RPLD_SERVER'};

 if ( defined($server) && length($server) ) {
  return $e->connect($server);
 }

 while (!defined($e->[0]) && scalar(@locations)) {
  $e->connect(shift(@locations));
 }

 return defined($e->[0]) ? $e : undef;
}

=pod

=head3 $res = $rpld-E<gt>connect_default()

Connect to a server by trying default locations. You must not call this on a already connected object.

=cut

sub disconnect {
 my ($e) = @_;

 close($e->[0]);

 $e->[0] = undef;
}

=pod

=head3 $res = $rpld-E<gt>disconnect()

Disconnect from the server. You may use $rpld->connect() again to connect to a (new/different) server.

=cut

sub is_connected {
 my ($e) = @_;

 return defined($e->[0]) ? $e : undef;
}

=pod

=head3 $res = $rpld-E<gt>is_connected()

Returns true value if the object is currently connected to a server or undef if not.

=cut

sub read {
 my ($e) = @_;
 my $r;

 return undef unless $e->[1] == 1;

 $r = readline($e->[0]);

 $r =~ s/\r?\n//;

 if ( $r eq '.' ) {
  $e->[1] = 0;
  return undef;
 }

 $r =~ s/^\.(\.+)$/$1/;

 return $r;
}

sub write {
 my ($e, $d) = @_;

 return undef unless $e->[1] == 2;

 if ( $d !~ /\n/ ) {
  $d .= '\n';
 }

 $d =~ s/^(\.+)$/.$1/;

 return (print {$e->[0]} $d);
}

sub send_eof {
 my ($e) = @_;

 return undef unless $e->[1] == 2;

 return (print {$e->[0]} ".\r\n");
}

sub request {
 my ($e, $req) = @_;

 return undef unless $e->[1] == 0;

 return (print {$e->[0]} $req, "\r\n");
}

sub response {
 my ($e) = @_;
 my $r;
 my $ar;

 return undef unless $e->[1] == 0;

 $r = readline($e->[0]);

 $r =~ s/\r?\n//;

 $r =~ /^>\s+(\d+)\s+(.+)$/ or return undef;
 $ar = [int($1), $2];

 if ( wantarray() ) {
  return @{$ar};
 } else {
  return $ar->[0];
 }
}

sub cmd {
 my ($e, $cmd, @args) = @_;

 return undef unless $e->[1] == 0;

 if ( scalar(@args) == 1 && !defined($args[0]) ) {
  @args = ();
 }

 if ( scalar(@args) ) {
  $cmd .= ' ';
  $cmd .= join(' ', @args);
 }

 $e->request($cmd);
 $e->[1] = 1;
 while ($e->read($e)) {}

 return $e->response();
}

sub cmd_data {
 my ($e, $cmd, @args) = @_;
 my $t;
 my @r = (undef);

 return undef unless $e->[1] == 0;

 if ( scalar(@args) == 1 && !defined($args[0]) ) {
  @args = ();
 }

 if ( scalar(@args) ) {
  $cmd .= ' ';
  $cmd .= join(' ', @args);
 }

 $e->request($cmd);
 $e->[1] = 1;
 push(@r, $t) while ($t = $e->read($e));

 $r[0] = $e->response();

 return \@r;
}

#-------------
# Return values

sub is_ok {
 return 0 unless defined($_[1]);

 return 1 if $_[1] == 0;
 return 0;
}

sub is_error {
 return 1 unless defined($_[1]);

 return 1 if $_[1] == 1;
 return 0;
}

sub is_yes {
 return 3 unless defined($_[1]);

 return 1 if $_[1] == 3;
 return 0;
}

sub is_no {
 return 4 unless defined($_[1]);

 return 1 if $_[1] == 4;
 return 0;
}

#-------------
# consts

=pod

=head2 Special values

=cut

sub any {
 return \'ANY';
}

=pod

=head3 $any = $rpld-E<gt>any()

This function returns a value hat can be used as playlist wildcard.

=cut

#-------------
# quotes

sub q_str {
 my ($e, $s) = @_;
 my %q = ('\\' => '\\\\', '"' => '\"');
 my $g = join('|', map{quotemeta}(keys(%q)));

 $s =~ s/($g)/$q{$1}/g;

 return '"'.$s.'"';
}

sub q_pli {
 my ($e, $pl) = @_;

 return undef unless defined $pl;

 if ( $pl =~ /^\d+$/ ) {
  return int($pl);
 } elsif ( ref($pl) eq 'SCALAR' ) {
  return ${$pl};
 }

 return $e->q_str($pl);
}

sub q_ple {
 my ($e, $ple) = @_;

 return $ple;
}

sub q_iot {
 my ($e, $io) = @_;

 return $io;
}

sub q_plt {
 my ($e, $plt) = @_;

 return uc($plt);
}

#-------------
# parsers:

sub p_ple {
 my ($e, $ple) = @_;
 my $r = {};
 my @p = split(/=/, $ple);
 my @t;
 local $_;

 #0       1        2     3     4      5         6       7    8                                                                                  9           10
 #unknown=00:00:00=ALBUM=TITLE=ARTIST=PERFORMER=VERSION=FILE=long:0xBED9000000000373/short:0xBE0003AA/uuid:0c26ea9c-5f37-48e3-b338-8895b1a84dfe=0xDISCID/TN=GENRE(GID)=LIKENESS

# $r->{'raw'} = {'data' => $ple, 'splited' => \@p};

 foreach (@p) {
  if ( $_ eq '*' ) {
   $_ = undef;
   next;
  }
  if ( $_ eq '' ) {
   $_ = undef;
  }
 }

 if ( $p[0] eq 'unknown' ) {
  $r->{'codec'}  = undef;
 } else {
  $r->{'codec'}  = $p[0];
 }

 @t = split(/:/, $p[1]);

 $r->{'length'} = $t[-1] + $t[-2] * 60 + $t[-3] * 3600;

 $r->{'length'} = undef unless $r->{'length'};

 $r->{'file'}   = $p[7];

 $r->{'meta'}   = {'album' => $p[2], 'title' => $p[3], 'artist' => $p[4], 'performer' => $p[5], 'version' => $p[6]};

 if ( $p[8] =~ m#^(long:0x[0-9a-fA-F]{16})/(short:0x[0-9a-fA-F]{8})(?:/(uuid:[0-9a-fA-F-]{36}))?$# ) {
  $r->{'longid'}  = $1;
  $r->{'shortid'} = $2;
  $r->{'uuid'}    = $3 if $3;
 }

 if ( $p[9] =~ m#^(0x[0-9a-fA-F]{8})/(\d+)$# ) {
  $r->{'meta'}->{'discid'}      = hex($1) if $1;
  $r->{'meta'}->{'tracknumber'} = int($2) if $2;
  $r->{'meta'}->{'discid'} = undef unless $r->{'meta'}->{'discid'};
  if ( $r->{'meta'}->{'discid'} ) {
   $r->{'meta'}->{'totaltracks'} = $r->{'meta'}->{'discid'} & 0xFF;
  }
 }

 if ( $p[10] =~ m#^(.+)\((0x[0-9a-fA-F]+)\)$# ) {
  if ( $2 !~ /^0xf+$/i ) {
   $r->{'meta'}->{'genre'}       = $1      if $1;
   $r->{'meta'}->{'genreid'}     = hex($2) if $2;
  }
 }

 if ( defined($p[11]) ) {
  $r->{'likeness'} = $p[11]+0;
 }

 return $r;
}

#-------------
# define hi-level functions

# -- Basic:

=pod

=head2 Basic communication functions

=cut

sub noop {
 return $_[0]->cmd('NOOP');
}

=pod

=head3 $res = $rpld-E<gt>noop()

Send a NOOP command to the server.

This can be used to ping the server or for keep-alive.

=cut

# -- Store/Restore

=pod

=head2 Storing and restoring

=cut

sub store {
 my $r = $_[0]->cmd('STORE');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>store()

Ask the server to store (dump) all data on permanent storage (disk). This is normally used at exit to save all the playlists on disk so they can be restored at next startup.

=cut

sub restore {
 my $r = $_[0]->cmd('RESTORE');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>restore()

Restore from disk. This is normally done at startup time (by rpld itself). You should not call this again if the server already restored it's state because this may result in undefined behavior. Current behavior of rpld is that it adds everything to the current state resulting in duplicates. This may change in future.

=cut

# -- Queue:

=pod

=head2 Playback control

=cut

sub play {
 my $r = $_[0]->cmd('PLAY');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>play()

Start playback.

=cut

sub stop {
 my $r = $_[0]->cmd('STOP');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>stop()

Stop playback.

=cut

sub next {
 my $r = $_[0]->cmd('NEXT');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>next()

Skip to next song.

=cut

sub prev {
 my $r = $_[0]->cmd('PREV');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>prev()

Skip to previous song.

=cut

sub isplaying {
 my $r = $_[0]->cmd('ISPLAYING');

 return undef unless defined($r);

 return $_[0]->is_yes($r);
}

=pod

=head3 $res = $rpld-E<gt>isplaying()

Return a true value if we are currently playing.

=cut

=pod

=head2 Queue control

=cut

sub flushq {
 my $r = $_[0]->cmd('FLUSHQ');

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>flushq()

Flush the Main Queue. Playback is stopped (because no songs are left in the queue).

=cut

sub showcur {
 my ($e) = @_;
 my $q = $e->cmd_data('SHOWCUR');

 return undef unless $e->is_ok($q->[0]);

 return $e->p_ple($q->[1]);
}

=pod

=head3 $res = $rpld-E<gt>showcur()

Return the Playlist entry for the current song (if playback is running this is the currently played song).
This contains information about this song from the playlist. If you want to read the current meta data or other live data use $rpld->showplaying().

The return value is in the same format as that of $rpld->showple(). See $rpld->showple() for more information on it.

=cut

sub showplaying {
 my ($e) = @_;
 my $q = $e->cmd_data('SHOWPLAYING');
 my $r = {'meta' => {}};
 my ($k, $v);
 local $_;

 return undef unless $e->is_ok(shift(@{$q}));

 foreach (@{$q}) {
  /^(\S+)\s+(\S.*)$/ or return undef;
  ($k, $v) = (lc($1), $2);

  if ( $k eq 'state' ) {
   $r->{$k} = uc($v);
  } elsif ( $k eq 'uuid' || $k eq 'longgtn' || $k eq 'shortgtn' ) {
   $r->{$k} = lc($v);
  } elsif ( $k eq 'time' ) {
   $v =~ /^(\d+) S(?: \(([0-9\.]+)s\))?$/;
   $r->{'time'} = {'samples' => $1, ($2 ? ('s' => $2+0) : ())};
  } elsif ( $k eq 'meta' ) {
   /^META (\S+)\s+"(.*)"$/i or return undef;
   $r->{'meta'}->{lc($1)} = $2;
  } else {
   $r->{$k} = int($v);
  }
 }

 if ( exists($r->{'meta'}->{'discid'}) ) {
  $r->{'meta'}->{'totaltracks'} = $r->{'meta'}->{'discid'} & 0xFF;
 }

 return $r;
}

=pod

=head3 $res = $rpld-E<gt>showplaying()

Returns live information on what is currently played.

The return value is a hashref which contains the following keys (all keys may or may not be set depending on what info the server provides):

=over

=item state

A string of the current playback state in uppercase. Possible values include: STOPPED, PAUSE, RUNNING.

=item longgtn

The long GTN of the current ple.

=item shortgtn

The short GTN of the current ple.

=item uuid

The UUID of the current ple.

=item time

A hash containing keys for the current playback time in different units. The unit name is the key name. Units may include: samples, s.

=item meta

A hash containing keys for each meta data type provided by the server. Types may include: title, album, artist, performer, version.

=item mduc

The meta data update counter (MDUC) for current playback.

=item rate

The sample rate of the current playback.

=item channels

The number of channels of the current playback.

=item bits

The number of bits of the current playback.

=back

=cut

sub listq {
 return $_[0]->listple($_[1] || 0); # Main Queue = 0
}

=pod

=head3 $res = $rpld-E<gt>listq([$playlist])

List playlist entries of the Main Queue or the given playlist.
This is the same as $rpld->listple() expect that the default playlist is the Main Queue not the currently selected playlist.

=cut

=pod

=head2 Volume control

=cut

sub setvolume {
 my ($e, $vol) = @_;
 my $r;
 #  SETVOLUME {NUM|PC%}

 if ( $vol =~ /^\d+\%$/ ) {
  $r = $e->cmd('SETVOLUME', $vol);
 } else {
  $r = $e->cmd('SETVOLUME', int($vol));
 }

 return $e->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>setvolume($vol)

Set volume to $vol.

=cut

sub showvolume {
 my ($e) = @_;
 my $q = $e->cmd_data('SHOWVOLUME');
 my $r = {};

 return undef unless $e->is_ok($q->[0]);

 $q->[1] =~ /^VOLUME (\d+)\/(\d+) (\d+)%$/ or return undef;

 %{$r} = ('value' => $1, 'scale' => $2, 'pc' => $3);

 return $r;
}

=pod

=head3 $res = $rpld-E<gt>showvolume()

Return a hashref to a hash containing volume information.

The hash contains the following keys:

=over

=item value

The current playback volume in units of scale.

=item scale

The unit of value.

=item pc

The volume in percent.

=back

The current volume (as float) is value/scale.

=cut

=pod

=head2 Controlling pause state

=cut

sub pause {
 my ($e) = @_;

 return $e->is_ok($e->cmd('PAUSE', 'TRUE'));
}

=pod

=head3 $res = $rpld-E<gt>pause()

Set the playback in pause mode.

You should not use this function but $rpld->togglepause() if possible.

=cut

sub unpause {
 my ($e) = @_;

 return $e->is_ok($e->cmd('PAUSE', 'FALSE'));
}

=pod

=head3 $res = $rpld-E<gt>unpause()

Unpause the playback.

You should not use this function but $rpld->togglepause() if possible.

=cut

sub togglepause {
 my ($e) = @_;

 return $e->is_ok($e->cmd('PAUSE', 'TOGGLE'));
}

=pod

=head3 $res = $rpld-E<gt>togglepause()

Toggle the pause state. This is the recommended function to change the pause state of playback.

This is because it interacts best with other applications changing the current pause state.

=cut

# --- Playlists:

=pod

=head2 Playlist management

=cut

sub setplaylist {
 my ($e, $pl) = @_;
 my $r = $_[0]->cmd('SETPLAYLIST', $e->q_pli($pl));

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>setplaylist($playlist)

Select the given playlist. After selecting a playlist it becomes the default playlist for most operations which take a optional playlist argument. (Operations for which this does not set the default are marked in the corresponding description).

=cut

sub addplaylist {
 my ($e, $pl) = @_;
 my $r = $_[0]->cmd('ADDPLAYLIST', $e->q_str($pl));

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>addplaylist($name)

Add a playlist.

=cut

sub delplaylist {
 my ($e, $pl) = @_;
 my $r = $_[0]->cmd('DELPLAYLIST', $e->q_pli($pl));

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>delplaylist($playlist)

Delete a playlist. All entries are deleted, too.

=cut

sub flushplaylist {
 my ($e, $pl) = @_;
 my $r = $_[0]->cmd('FLUSHPLAYLIST', $e->q_pli($pl));

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>flushplaylist($playlist)

Flush a playlist. If this is used on the Main Queue the playback is stopped as no songs are left to play.

=cut

sub listplaylists {
 my ($e) = @_;
 my $q = $e->cmd_data('LISTPLAYLISTS');
 my @r;
 my $c;
 my %idcache;
 local $_;

 return undef unless defined($q);

 return undef unless $e->is_ok(shift(@{$q}));

 while (($_ = shift(@{$q}))) {
  /^\s*(\d+):\s*\[parent:\s*(\d+)\]\s*"(.+?)"$/ or next;
  $c = {'id' => int($1), 'parent' => int($2), 'name' => $3, 'children' => []};
  $idcache{$c->{'id'}} = $c;
  push(@r, $c);
 }

 foreach (keys(%idcache)) {
  $c = $idcache{$_};

  next if $c->{'id'} == $c->{'parent'};
  next if $c->{'parent'} == 0;

  push(@{$idcache{$c->{'parent'}}->{'children'}}, $c);
 }

 return \@r;
}

=pod

=head3 $res = $rpld-E<gt>listplaylist()

Get a list of playlists known by the server.
This returns a arrayref to a array containing a hashref for each list. This hash contains the following keys:

=over

=item id

The ID of the playlist.

=item parent

The ID of the parent playlist.

=item name

The name of the playlist.

=item children

A arrayref with the IDs of the children playlists.

=back

Parent/child information should be used to display a tree to the user.

=cut

sub showlist {
 my ($e, $pl) = @_;
 my $q = $e->cmd_data('SHOWLIST', $e->q_pli($pl));

 return undef unless $e->is_ok($q->[0]);

 $q->[1] =~ /^\s*(\d+):\s*\[parent:\s*(\d+)\]\s*"(.+?)"$/ or return undef;

 return {'id' => int($1), 'parent' => int($2), 'name' => $3};
}

=pod

=head3 $res = $rpld-E<gt>showlist($playlist)

Returns a hashref with information about the given playlist:

=over

=item id

The ID of the playlist.

=item parent

The ID of the parent playlist.

=item name

The name of the playlist.

=back

=cut

sub setparentlist {
 my ($e, $pl) = @_;
 my $r = $_[0]->cmd('SETPARENTLIST', $e->q_pli($pl));

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>setparentlist($playlist)

Set the parent playlist of the current playlist.
Setting parent playlist of a list the current one is currently not supported.

=cut

# -- PLE:

=pod

=head2 Playlist Entry management

=cut

sub delple {
 my ($e, $ple, $pl) = @_;
 my @q = ($e->q_ple($ple));
 my $r;

 if ( defined($pl) ) {
  push(@q, 'FROM');
  push(@q, $e->q_pli($pl));
 }

 $r = $_[0]->cmd('DELPLE', @q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>delple($ple[, $playlist])

Delete the given playlist entry.

=cut

sub queueple {
 my ($e, $ple, $pl, $pos) = @_;
 my @q = ($e->q_ple($ple));
 my $r;

 if ( defined($pl) ) {
  push(@q, 'FROM');
  push(@q, $e->q_pli($pl));
 }

 if ( defined($pos) ) {
  push(@q, 'AT');
  push(@q, int($pos));
 }

 $r = $_[0]->cmd('QUEUEPLE', @q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>queueple($ple[, $playlist[, $pos]])

Queue the given playlist entry. This means it is copied over to the Main Queue. This function is recommended over $rpld->copyple() to queue songs.

The optional position is used to give the place in the playlist where to queue the song.
It is counted as 'n entries after the first one'. This means if you pass a value of zero the song is placed as next song. If you pass a value of one it is placed as the song after the next song and so on. A value of -1 means that it is added before the first one. If you place a song before the first one in the Main Queue playback is stopped and restarted so this song becomes the currently played song.

If you omit the position or pass undef the default behavior is to add at the end of the playlist which should be the default in your applications, too.

=cut

sub copyple {
 my ($e, $ple, $fpl, $tpl, $pos) = @_;
 my @q = ($e->q_ple($ple));
 my $r;

 if ( !defined($tpl) ) {
  $tpl = $fpl;
  $fpl = undef;
 }

 if ( defined($fpl) ) {
  push(@q, 'FROM');
  push(@q, $e->q_pli($fpl));
 }

 push(@q, 'TO');
 push(@q, $e->q_pli($tpl));

 if ( defined($pos) ) {
  push(@q, 'AT');
  push(@q, int($pos));
 }

 $r = $_[0]->cmd('COPYPLE', @q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>copyple($ple, $playlist_from, $playlist_to[, $pos])

The given playlist entry is copied from the playlist $playlist_from to the playlist $playlist_to.
If $playlist_from is undef the currently selected playlist is used as source.

For the meaning of the position parameter ($pos) see $rpld->queueple() above.

=cut

sub moveple {
 my ($e, $ple, $fpl, $tpl, $pos) = @_;
 my @q = ($e->q_ple($ple));
 my $r;

 if ( !defined($tpl) ) {
  $tpl = $fpl;
  $fpl = undef;
 }

 if ( defined($fpl) ) {
  push(@q, 'FROM');
  push(@q, $e->q_pli($fpl));
 }

 push(@q, 'TO');
 push(@q, $e->q_pli($tpl));

 if ( defined($pos) ) {
  push(@q, 'AT');
  push(@q, int($pos));
 }

 $r = $_[0]->cmd('MOVEPLE', @q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>moveple($ple, $playlist_from, $playlist_to[, $pos])

The given playlist entry is moved from the playlist $playlist_from to the playlist $playlist_to.
If $playlist_from is undef the currently selected playlist is used as source.

For the meaning of the position parameter ($pos) see $rpld->queueple() above.

=cut

sub listple {
 my ($e, $pl) = @_;
 my $q = $e->cmd_data('LISTPLE', $e->q_pli($pl));
 my @r;
 local $_;

 return undef unless $e->is_ok(shift(@{$q}));

 foreach (@{$q}) {
  push(@r, $e->p_ple($_));
 }

 return \@r;
}

=pod

=head3 $res = $rpld-E<gt>listple($playlist)

List all playlist entries from the given playlist.
The return value is an arrayref with elements as if they are returned by $rpld->showple().
See $rpld->showple() for more info about the return format.

Note:
You should avoid calling this too often as it returns a large amount of data. You also should not store data for
currently not used playlists (for example only store data for the playlist currently shown to the user).

=cut

sub showple {
 my ($e, $ple, $pli) = @_;
 my @args = ($e->q_ple($ple));
 my $q;

 if ( $pli ) {
  push(@args, 'FROM');
  push(@args, $e->q_pli($pli));
 }

 $q = $e->cmd_data('SHOWPLE', @args);

 return undef unless $e->is_ok($q->[0]);

 return $e->p_ple($q->[1]);
}

=pod

=head3 $res = $rpld-E<gt>showple($ple[, $playlist])

Return data of the given playlist entry. If no playlist is given the currently selected one is used.

The return value is a hashref with the following keys:

=over

=item codec (optional)

The name of the used codec.

=item length (optional)

The playback length in sec.

=item file

The filename. This may be anything supported by RoarAudio's DSTR. Including local files, web radio streams and other types.

=item meta

The value for the meta key is a hashref of it's own to a list of provided meta data.

The following keys may be included. All are optional. Other keys may also be included.

=over 8

=item album

Name of the album.

=item title

Title of the song.

=item artist

Name of the Artist.

=item performer

Name of the performer for this record.

=item version

Version of this record.

=item discid

CDDB DiscID for this song.

=item tracknumber

Tracknumber of this song in the album.

=item totaltracks

Total number of tracks in this album.

=item genre

Genre of this song.

=item genreid

Genre ID of this song.

=back

=item longid

The long GTN for the entry.

=item shortid

The short GTN for the entry.

=item uuid (optional)

The UUID for the entry.

=item likeness (optional)

The likeness value stored by the server.
This is a float in rage from zero to infinity.
The bigger the value is the more the song is liked.

=back

=cut

sub like {
 my ($e, $ple, $likeness) = @_;
 my @q = ('LIKE', $e->q_ple($ple), ($likeness+0 || 1));
 my $r = $e->cmd(@q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>like($ple[, $likeness])

Tells the server that the user likes this entry.
Optionally tells the server how much. A value of +1.0 is the default if no value is given.
This is added to the likeness value stored by the server.
A value of zero has no effect. A negative value indecates dislikeness.
See dislike() for more information about dislikeness.

=cut

sub dislike {
 my ($e, $ple, $likeness) = @_;
 my @q = ('DISLIKE', $e->q_ple($ple), $likeness+0 || 1);
 my $r = $e->cmd(@q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>dislike($ple[, $likeness])

This is the same as like() just markes a the given entry as disliked.
Optionally tells the server how much. A value of +1.0 is the default if no value is given.
The value is subtracted from server's value.
A value of zero has no effect. A negative value indecates likeness.
See like() for more information about likeness.

=cut

# -- POINTER:

=pod

=head2 Pointers

=cut

sub setpointer {
 my ($e, $pointer, $ple, $pli) = @_;
 my @q = (uc($pointer), $e->q_ple($ple));
 my $r;

 if ( $pli ) {
  push(@q, 'FROM');
  push(@q, $e->q_pli($pli));
 }

 $r = $_[0]->cmd('SETPOINTER', @q);

 return undef unless defined($r);

 return $_[0]->is_ok($r);
}

=pod

=head3 $res = $rpld-E<gt>setpointer($pointer, $ple[, $playlist])

Set the given pointer to the given playlist entry. If no playlist is given the currently selected one is used.

=cut

sub showpointer {
 my ($e, $pointer) = @_;
 my $q;
 my $r = {};
 my $plent;
 local $_;

 $q = $e->cmd_data('SHOWPOINTER', $pointer ? (uc($pointer)) : ());

 return undef unless $e->is_ok(shift(@{$q}));

 foreach (@{$q}) {
  if ( /^POINTER (.+) NOT SET$/ ) {
   $r->{$1} = {};
  } elsif ( /^POINTER (.+?) IS AT (.+)/ ) {
   $r->{$1} = {'plent' => $plent = {'raw' => $2}};
   if ( $plent->{'raw'} =~ /^long:/ ) {
    $plent->{'longid'} = $plent->{'raw'};
   } elsif ( $plent->{'raw'} =~ /^short:/ ) {
    $plent->{'shortid'} = $plent->{'raw'};
   } elsif ( $plent->{'raw'} =~ /^uuid:/ ) {
    $plent->{'uuid'} = $plent->{'raw'};
   }
  }
 }

 return $r;
}

=pod

=head3 $res = $rpld-E<gt>showpointer([$pointer])

Returns informations about the given or all pointers.

The return value is a hashref with keys of the pointer names.
The values for those keys are a hashref containing information on the corresponding pointer.
If the hashref for the pointer points to a empty hash (no keys defined) then the pointer is not defined.

If the pointer is defined the following keys are contained:

=over

=item raw

The playlist entry the pointer points to in a raw format.

=item longid (optional)

If the pointer contains an information about the playlist entries long GTN this GTN.

=item shortid (optional)

If the pointer contains an information about the playlist entries short GTN this GTN.

=item uuid (optional)

If the pointer contains an information about the playlist entries UUID this UUID.

=back

=cut

1;

=pod
=head1 SEE ALSO

=head1 AUTHOR

  Philipp "ph3-der-loewe" Schafft <lion@lion.leolix.org>

=head1 LICENSE

      Copyright (C) Philipp 'ph3-der-loewe' Schafft - 2009-2010
  
  This file is part of Audio::RPLD,
  a library to access the RoarAudio PlayList Daemon from Perl.
  See README for details.
  
  This file is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 3
  as published by the Free Software Foundation.
  
  Audio::RPLD is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this software; see the file COPYING.gplv3.
  If not, write to the Free Software Foundation, 51 Franklin Street,
  Fifth Floor, Boston, MA 02110-1301, USA.

=cut

__DATA__
Commands:
  HELP
  SHOWIDENTIFIER
  ADD2Q entry
  DELFQ identifier
  QUIT
  IMPORT [{"Name"|ID}] {TO|FROM} {STDIN|STDOUT|"Filename"} [AS {RPLD|PLS|M3U|VCLT|XSPF|PLAIN}]
  EXPORT [{"Name"|ID}] {TO|FROM} {STDIN|STDOUT|"Filename"} [AS {RPLD|PLS|M3U|VCLT|XSPF|PLAIN}]
Commands to fix:
  SETPARENTLIST [OF {"Name"|ID}] [TO] {"Name"|ID}
Commands to implement:
  SEARCHPLE "search string" [{AS|IN} {ALL|TITLE|ARTIST|VERSION|FILENAME|TAG:"Tagname"}] [FROM {"Name"|ID}]
  STORE {NONE|CONFIG|ALL|QUEUE|PLAYLIST {"Name"|ID}}
  RESTORE {NONE|CONFIG|ALL|QUEUE|PLAYLIST {"Name"|ID}}
