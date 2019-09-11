--- Tests for the expect module.
-- @license GPL-2.0-or-later
-- @author John Erling Blad < jeblad@gmail.com >

local testframework = require 'Module:TestFramework'

local Expect = require 'expect'
assert( Expect )

local function testExists()
	return type( Expect )
end

local function makeExpect( ... )
	return Expect:create( ... )
end

local function testCreate( ... )
	return type( makeExpect( ... ) )
end

local function testIsSoft( ... )
	return makeExpect( ... )
		:isSoft()
end

local function testHasSoft( ... )
	return makeExpect( ... )
		:hasSoft()
end

local function testHasName( ... )
	return makeExpect( ... )
		:hasName()
end

local function testGetName( ... )
	return makeExpect( ... )
		:getName()
end

local function testCompare( name, ... )
	return makeExpect( name )
		:addProcess(function( ... ) return ... end)
		:compare( ... )
end

local function testEval( name, bool, ... )
	return pcall(
		function( ... )
			return makeExpect( name, bool )
				:addProcess(function( ... ) return ... end)
				:eval( nil, ... )
		end,
		... )
end

local function testCallback( ... )
	return pcall(
		function( ... )
			local pass = false
			local fail = false
			local function passCb()
				pass = true
			end
			local function failCb()
				fail = true
			end
			local t = {
				makeExpect( true, passCb, failCb )
					:equal( true )
					:compare( ... ) }
			return pass, fail, unpack( t )
		end,
		... )
end

local function testReport( ... )
	return pcall(
		function( ... )
			local reports = {}
			local t = {
				makeExpect( true )
					:addFailReport( reports, 'fail' )
					:addPassReport( reports, 'pass' )
					:equal( true )
					:compare( ... ) }
			return unpack( reports ), unpack( t )
		end,
		... )
end

local function testSimpleProcs( procs, ... )
	local instance = makeExpect( true )
	for _,v in ipairs( procs ) do
		local fname = table.remove( v, 1 )
		instance[fname]( instance, unpack( v ) )
	end
	return instance:compare( ... )
end

local function testFilter( ... )
	local function f( _, arg )
		return type( arg ) == 'string'
	end
	return makeExpect( true )
		:filter( f )
		:asType()
		:equal( 'string' ):compare( ... )
end

local function testMap( ... )
	local function f( _, arg )
		return type( arg )
	end
	return makeExpect( true )
		:map( f )
		:equal( 'string' )
		:compare( ... )
end

--- Build test functions from definitions
local function makeTestPicks( proc, num, wrong )
	local strs = {
		'one', 'two', 'three',
		'four', 'five', 'six',
		'seven', 'eight', 'nine',
		'ten', 'eleven', 'twelve'
	}
	return {
		name = mw.ustring.format('Testing graph with "%s" (%s) with multiple strings, and compare with equality',
			proc,
			wrong or strs[num] ),
		func = testSimpleProcs,
		args = { { { proc }, { 'toBeEqual', wrong or strs[num] } }, unpack( strs ) },
		expect = { not wrong }
	}
end

--- Build test functions from definitions
local function makeTestTransforms( proc, arg, out )
	return {
		name = mw.ustring.format('Testing graph with "%s", arg of type "%s", and compare with equality',
			proc,
			type( arg ) ),
		func = testSimpleProcs,
		args = { { { proc }, { 'toBeEqual', out } }, arg },
		expect = { true }
	}
end

--- Build test functions from definitions
local function makeTestConditions( proc, arg, out, res )
	return {
		name = mw.ustring.format('Testing graph with arg of type "%s", and compare with "%s"',
			type( arg ),
			proc ),
		func = testSimpleProcs,
		args = { { { proc, out } }, arg },
		expect = { res }
	}
end

local tests = {
	{ -- 1
		name = 'Verify the lib is loaded and exists',
		func = testExists,
		type = 'ToString',
		expect = { 'table' }
	},
	{ -- 2
		name = 'Create with nil argument',
		func = testCreate,
		type = 'ToString',
		args = { nil },
		expect = { 'table' }
	},
	{ -- 3
		name = 'Create with single string argument',
		func = testCreate,
		type = 'ToString',
		args = { 'a' },
		expect = { 'table' }
	},
	{ -- 4
		name = 'Calling method "isSoft" with single boolean false argument',
		func = testIsSoft,
		args = { false },
		expect = { false }
	},
	{ -- 5
		name = 'Calling method "isSoft" with single boolean true argument',
		func = testIsSoft,
		args = { true },
		expect = { true }
	},
	{ -- 6
		name = 'Calling method "hasSoft" without argument',
		func = testHasSoft,
		args = {},
		expect = { false }
	},
	{ -- 7
		name = 'Calling method "hasSoft" with single boolean false argument',
		func = testHasSoft,
		args = { false },
		expect = { true }
	},
	{ -- 8
		name = 'Calling method "hasSoft" with single boolean true argument',
		func = testHasSoft,
		args = { true },
		expect = { true }
	},
	{ -- 9
		name = 'Calling method "hasName" without argument',
		func = testHasName,
		args = {},
		expect = { false }
	},
	{ -- 10
		name = 'Calling method "hasName" with single string "foo" argument',
		func = testHasName,
		args = { 'foo' },
		expect = { true }
	},
	{ -- 11
		name = 'Calling method "getName" with single string "foo" argument',
		func = testGetName,
		args = { 'foo' },
		expect = { 'foo' }
	},
	{ -- 12
		name = 'Calling method "compare" with name and single boolean false argument',
		func = testCompare,
		args = { 'test', false },
		expect = { false, 'test' }
	},
	{ -- 13
		name = 'Calling method "compare" with name and single boolean true argument',
		func = testCompare,
		args = { 'test', true },
		expect = { true, 'test' }
	},
	{ -- 14
		name = 'Calling method "compare" with name and multiple boolean true arguments',
		func = testCompare,
		args = { 'test', true, true, true },
		expect = { true, 'test' }
	},
	{ -- 15
		name = 'Calling method "compare" with name and multiple boolean true arguments, with final false',
		func = testCompare,
		args = { 'test', true, true, true, false },
		expect = { false, 'test' }
	},
	{ -- 16
		name = 'Calling method "compare" with name and multiple boolean true argument, with initial false',
		func = testCompare,
		args = { 'test', false, true, true, true },
		expect = { false, 'test' }
	},
	{ -- 17
		name = 'Calling method "eval" with name, without soft handling, and single boolean false argument',
		func = testEval,
		args = { 'test', false, false },
		expect = { false, 'Failed expectation' }
	},
	{ -- 18
		name = 'Calling method "eval" with name, without soft handling, and single boolean true argument',
		func = testEval,
		args = { 'test', false, true },
		expect = { true, true, 'test' }
	},
	{ -- 19
		name = 'Calling method "eval" with name, with soft handling, and single boolean false argument',
		func = testEval,
		args = { 'test', true, false },
		expect = { true, false, 'test' }
	},
	{ -- 20
		name = 'Calling method "eval" with name, with soft handling, and single boolean true argument',
		func = testEval,
		args = { 'test', true, true },
		expect = { true, true, 'test' }
	},
	{ -- 21
		name = 'Testing method "compare" with callbacks, with soft handling, and single boolean false argument',
		func = testCallback,
		args = { false },
		expect = { true, false, true, false }
	},
	{ -- 22
		name = 'Testing method "compare" with callbacks, with soft handling, and single boolean true argument',
		func = testCallback,
		args = { true },
		expect = { true, true, false, true }
	},
	{ -- 21
		name = 'Testing method "compare" with reports, with soft handling, and single boolean false argument',
		func = testReport,
		args = { false },
		expect = { true, 'fail', false }
	},
	{ -- 22
		name = 'Testing method "compare" with reports, with soft handling, and single boolean true argument',
		func = testReport,
		args = { true },
		expect = { true, 'pass', true }
	},
	{ -- 23
		name = 'Testing empty graph',
		func = testSimpleProcs,
		args = { {} },
		expect = { true }
	},
	{ -- 24
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'pick', 1 }, { 'toBeEqual', 'foo' } }, 'foo', 'bar' },
		expect = { true }
	},
	{ -- 25
		name = 'Testing graph with "pick" (2) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'pick', 2 }, { 'toBeEqual', 'bar' } }, 'foo', 'bar' },
		expect = { true }
	},
	{ -- 26
		name = 'Testing graph with "pick" (1,2) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'pick', 1, 2 }, { 'toBeEqual', 'foo', 'bar' } }, 'foo', 'bar' },
		expect = { true }
	},
	{ -- 27
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'pick', 1 }, { 'toBeEqual', 'baz' } }, 'foo', 'bar' },
		expect = { false }
	},
	{ -- 28
		name = 'Testing graph with "pick" (3) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'pick', 3 }, { 'toBeEqual', 'baz' } }, 'foo', 'bar' },
		expect = { false }
	},
	{ -- 29
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'toBeWithin', 0.01, 0.1 } }, 0.109 },
		expect = { true }
	},
	{ -- 30
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'toBeWithin', 0.01, 0.1 } }, 0.111 },
		expect = { false }
	},
	{ -- 31
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'asFraction' }, { 'toBeWithin', 0.01, 0.1 } }, 1.109 },
		expect = { true }
	},
	{ -- 32
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'asFraction' }, { 'toBeWithin', 0.01, 0.1 } }, 1.111 },
		expect = { false }
	},
	{ -- 33
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'asInteger' }, { 'toBeWithin', 0.1, 1 } }, 1.1 },
		expect = { true }
	},
	{ -- 34
		name = 'Testing graph with "pick" (1) from multiple strings, and compare with equality',
		func = testSimpleProcs,
		args = { { { 'asInteger' }, { 'toBeWithin', 0.1, 1 } }, 2.1 },
		expect = { false }
	},
	{ -- 35
		name = 'Testing graph with "filter" from multiple args, and compare with equality',
		func = testFilter,
		args = { false, 42, true },
		expect = { false } -- should be false as there are no strings in the args
	},
	{ -- 36
		name = 'Testing graph with "filter" from multiple args, and compare with equality',
		func = testFilter,
		args = { 'a', false, 'b', 'c', 42 },
		expect = { true } -- should be true as there are only strings in the filtered args
	},
	{ -- 37
		name = 'Testing graph with "map" over multiple args of different types, and compare with equality',
		func = testMap,
		args = { false, 42, true },
		expect = { false } -- should be false as there are no strings in the args
	},
	{ -- 38
		name = 'Testing graph with "map" over multiple args of string type, and compare with equality',
		func = testMap,
		args = { 'a', 'b', 'c' },
		expect = { true } -- should be true as there are only strings in the args
	},
	-- 39
	makeTestPicks( 'first', 1 ),
	makeTestPicks( 'first', 1, 'foo' ),
	makeTestPicks( 'second', 2 ),
	makeTestPicks( 'second', 2, 'foo' ),
	makeTestPicks( 'third', 3 ),
	makeTestPicks( 'third', 3, 'foo' ),
	makeTestPicks( 'fourth', 4 ),
	makeTestPicks( 'fourth', 4, 'foo' ),
	-- 47
	makeTestPicks( 'fifth', 5 ),
	makeTestPicks( 'fifth', 5, 'foo' ),
	makeTestPicks( 'sixth', 6 ),
	makeTestPicks( 'sixth', 6, 'foo' ),
	makeTestPicks( 'seventh', 7 ),
	makeTestPicks( 'seventh', 7, 'foo' ),
	makeTestPicks( 'eight', 8 ),
	makeTestPicks( 'eight', 8, 'foo' ),
	makeTestPicks( 'ninth', 9 ),
	makeTestPicks( 'ninth', 9, 'foo' ),
	-- 57
	makeTestPicks( 'tenth', 10 ),
	makeTestPicks( 'tenth', 10, 'foo' ),
	makeTestPicks( 'eleventh', 11 ),
	makeTestPicks( 'eleventh', 11, 'foo' ),
	makeTestPicks( 'twelfth', 12 ),
	makeTestPicks( 'twelfth', 12, 'foo' ),
	-- 63
	makeTestTransforms( 'asType', nil, 'nil' ),
	makeTestTransforms( 'asType', false, 'boolean' ),
	makeTestTransforms( 'asType', true, 'boolean' ),
	makeTestTransforms( 'asType', 42, 'number' ),
	-- 67
	makeTestTransforms( 'asType', 'ping-pong', 'string' ),
	makeTestTransforms( 'asType', {}, 'table' ),
	makeTestTransforms( 'asType', function() end, 'function' ),
	makeTestTransforms( 'asUpper', 'foo', 'FOO' ),
	makeTestTransforms( 'asLower', 'FOO', 'foo' ),
	makeTestTransforms( 'asUpperFirst', 'foo', 'Foo' ),
	makeTestTransforms( 'asLowerFirst', 'FOO', 'fOO' ),
	makeTestTransforms( 'asReverse', 'abc', 'cba' ),
	makeTestTransforms( 'asUUpper', 'æøå', 'ÆØÅ' ),
	makeTestTransforms( 'asULower', 'ÆØÅ', 'æøå' ),
	-- 77
	makeTestTransforms( 'asUUpperFirst', 'æøå', 'Æøå' ),
	makeTestTransforms( 'asULowerFirst', 'ÆØÅ', 'æØÅ' ),
	makeTestTransforms( 'asUNFC', mw.ustring.char( 065, 768 ), mw.ustring.char( 192 ) ),  -- Á
	makeTestTransforms( 'asUNFD', mw.ustring.char( 192 ), mw.ustring.char( 065, 768 ) ),
	makeTestTransforms( 'asNumber', '42', 42 ),
	makeTestTransforms( 'asString', 42, '42' ),
	makeTestTransforms( 'asFloor', 1.9, 1 ),
	makeTestTransforms( 'asFloor', 2.0, 2 ),
	makeTestTransforms( 'asCeil', 2.0, 2 ),
	makeTestTransforms( 'asCeil', 2.1, 3 ),
	-- 87
	makeTestTransforms( 'asRound', 2.4, 2 ),
	makeTestTransforms( 'asRound', 2.5, 3 ),
	-- 89
	makeTestConditions( 'toBeEqual', false, true, false ),
	makeTestConditions( 'toBeEqual', false, false, true ),
	makeTestConditions( 'toBeEqual', true, true, true ),
	makeTestConditions( 'toBeEqual', 42, 42, true ),
	makeTestConditions( 'toBeEqual', 'foo', 'foo', true ),
	-- 94
	makeTestConditions( 'toBeBoolEqual', false, true, false ),
	makeTestConditions( 'toBeBoolEqual', true, true, true ),
	makeTestConditions( 'toBeBoolEqual', 42, true, true ),
	makeTestConditions( 'toBeBoolEqual', 'true', true, true ),
	-- 98
	makeTestConditions( 'toBeStrictEqual', 41, 42.0, false ),
	makeTestConditions( 'toBeStrictEqual', 42, 42.0, true ), -- this is a subtype in newer versions
	-- 100
	makeTestConditions( 'toBeSame', 41, 42, false ),
	makeTestConditions( 'toBeSame', 42, 42, true ),
	makeTestConditions( 'toBeSame', '42', 42, true ),
	makeTestConditions( 'toBeSame', 42, '42', true ),
	-- 104
	makeTestConditions( 'toBeDeepEqual', {}, {}, true ),
	makeTestConditions( 'toBeDeepEqual', { false }, { true }, false ),
	makeTestConditions( 'toBeDeepEqual', { true }, { true}, true ),
	makeTestConditions( 'toBeDeepEqual', { { 'a' } }, { { 'a' } }, true ),
	makeTestConditions( 'toBeDeepEqual', { { 'a' } }, { { { 'a' } } }, false ),
	makeTestConditions( 'toBeDeepEqual', { { 'a' } }, { { 'a' }, 'b' }, false ),
	-- 110
	makeTestConditions( 'toBeContained', { { { 'a' } } }, { { 'a' }, { { 'a' } } }, false ),
	makeTestConditions( 'toBeContained', { { { 'a' } } }, { { 'a' }, { { { 'a' } } } }, true ),
	makeTestConditions( 'toBeContained', { { { 'a' } } }, { { 'a' }, { { { { 'a' } } } } }, false ),
	-- 113
	makeTestConditions( 'toBeLessThan', 42, 43, false ),
	makeTestConditions( 'toBeLessThan', 42, 42, false ),
	makeTestConditions( 'toBeLessThan', 42, 41, true ),
	-- 116
	makeTestConditions( 'toBeGreatThan', 42, 43, true ),
	makeTestConditions( 'toBeGreatThan', 42, 42, false ),
	makeTestConditions( 'toBeGreatThan', 42, 41, false ),
	-- 119
	makeTestConditions( 'toBeLessOrEqual', 42, 43, false ),
	makeTestConditions( 'toBeLessOrEqual', 42, 42, true ),
	makeTestConditions( 'toBeLessOrEqual', 42, 41, true ),
	-- 122
	makeTestConditions( 'toBeGreatOrEqual', 42, 43, true ),
	makeTestConditions( 'toBeGreatOrEqual', 42, 42, true ),
	makeTestConditions( 'toBeGreatOrEqual', 42, 41, false ),
	-- 125
	makeTestConditions( 'toBeMatch', 'foo', 'bar', false ), -- pattern gives wrong result
	makeTestConditions( 'toBeMatch', 'foo', 'foo', true ), -- pattern gives wrong result
	-- 127
	makeTestConditions( 'toBeUMatch', 'føø', 'bar', false ), -- pattern gives wrong result
	makeTestConditions( 'toBeUMatch', 'føø', 'føø', true ), -- pattern gives wrong result
}

return testframework.getTestProvider( tests )
