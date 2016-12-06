local array = {
	'text';
	{
		'more text, in a nested table';
		1432
	};
	true -- booleans can also be in tables
}

local dictionary = {
	'text';
	text = 'more text';
	654;
	[8] = {}; -- an empty table
	func = function() print('tables can even contain functions!') end;
	['1213'] = 1213
}

return array [2][2] + dictionary ['1213']
