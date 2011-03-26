#!/usr/bin/perl
use strict;
use warnings;
use HTML::Entities;

print encode_entities(join('', <STDIN>), '&<>"\'');
