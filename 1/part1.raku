#!/usr/bin/env raku

'input.txt'.IO.lines>>.words.&zip>>.sort.&zip(with => &[-])>>.abs.sum.say;
