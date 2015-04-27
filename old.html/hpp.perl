# html pre-processor

$def = $ARGV[0];
$date = `date +'%Y-%m-%d %H:%M'`;

while (<STDIN>) {
    if (/^#ifdef ([-_A-Za-z0-9]+)/) {
	$supress = 1;
	$supress = 0 if ($1 eq $def);
    } elsif (/^#elifdef ([-_A-Za-z0-9]+)/) {
	$supress = 0 if ($1 eq $def && $supress);
    } elsif (/^#else/) {
	$supress = !$supress;
    } elsif (/^#endif/) {
	$supress = 0;
    } else {
	s/<modified>/$date/g;
	print unless $supress;
    }
}
