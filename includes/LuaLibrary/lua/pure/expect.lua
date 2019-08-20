--- Class for Expect.
-- This class follows the pattern from
-- [Lua classes](../topics/lua-classes.md.html).
-- @classmod Expect

-- @var class var for lib
local Expect = {}

local libUtil = require 'libraryUtil'

--- Lookup of missing class members.
-- @raise on wrong arguments
-- @tparam string key lookup of member
-- @return any
function Expect:__index( key ) -- luacheck: no self
	libUtil.checkType( 'Expect:__index', 1, key, 'string', false )
	return Expect[key]
end

--- Get a clone or create a new instance.
-- @function Expect:__call
-- @tparam vararg ... conditionally passed to create
-- @treturn self
function Expect:__call( ... )
	local instance = rawget( self, 'create' ) and self:create() or self
	local result = { instance:compare( ... ) } -- this should dispach values, not create the compute graph
	if not result[1] then
		if self._soft then
			return unpack( result )
		end
		error( result[1], 2 )
	end
	return unpack( result )
end

--- Create a new instance.
-- @tparam vararg ... forwarded to `_init()`
-- @treturn self
function Expect:create( ... )
	for i,v in ipairs( { ... } ) do
		libUtil.checkTypeMulti( 'Expect:_init', i, v, { 'string', 'boolean', 'table' } )
	end
	local meta = rawget( self, 'create' ) and self or getmetatable( self )
	local new = setmetatable( {}, meta )
	return new:_init( ... ) -- this should create the compute graph, not dispach values
end

--- Initialize a new instance.
-- @tparam vararg ... set to temporal
-- @treturn self
function Expect:_init( ... )
	self._processes = {}
	self._soft = false
	self._name = nil
	for i,v in ipairs( { ... } ) do
		local t = type( v )
		if t == 'string' then
			self._name = v
		elseif t == 'boolean' then
			self._soft = v
		elseif t == 'table' then
			self:import( v )
		end
	end
	return self
end

--- Import a compute grap.
-- @tparam table procs for the graph
-- @treturn self
function Expect:import( procs )
	for i,v in ipairs( procs ) do
		libUtil.checkType( 'Expect:__index', 1, v, 'function', false )
		table.insert( self.processes, v )
	end
	return self
end

--- Add a process function
-- @raise on wrong arguments
-- @tparam function proc to be evaluated
function Expect:addProcess( proc )
	libUtil.checkType( 'Expect:addProcess', 1, proc, 'function', false )
	table.insert( self._processes, proc )
	return self
end

--- Compare given values
-- @tparam varargs ... any used as arguments
-- @return list of any
function Expect:compare( ... )
	local tmp = { ... }
	for _,v in ipairs( self._processes ) do
		tmp = { pcall( v, unpack( tmp ) ) }
		if not tmp[1] then
			return false, self._name
		end
		table.remove( tmp, 1 )
	end
	for _,v in ipairs( tmp ) do
		if not v then
			return false, self._name
		end
	end
	return true, self._name
end

--- Eval given values
-- @tparam varargs ... any used as arguments
-- @return list of any
function Expect:eval( ... )
	local result = { self:compare( ... ) }
	if not result[1] then
		if self._soft then
			return unpack( result )
		end
		error( result[1], 2 )
	end
	return unpack( result )
end

--- Pick entries
function Expect:pick( ... )
	local idxs = { ... }
	local g = function( ... )
		local args = { ... }
		local t= {}
		for i,v in ipairs( idxs ) do
			t[i] = args[v]
		end
		return unpack(t)
	end
	self:addProcess( g )
	return self
end

--- Make a delayed process for the pick functions.
-- This is a private function that will create a function with a closure.
-- It will create an additional delayed function for the provided definition.
-- @local
-- @delayed
-- @raise on wrong arguments
-- @tparam number idx of the extracted item
-- @treturn function
local function _makePickProcess( idx )
	-- not public interface, but will verify if the defs are reasonable
	libUtil.checkType( '_makePickProcess', 1, idx, 'number', false )
	local g = function( ... )
		local t = { ... }
		return t[idx]
	end
	local f = function( self )
		self:addProcess( g )
		return self
	end
	return f
end

-- @var table of definitions for the picks
-- Format is `name = index`
local picks = {
	--- Make a pick for first item.
	-- @pick
	-- @function Expect:first
	first = 1,

	--- Make a pick for second item.
	-- @pick
	-- @function Expect:second
	second = 2,

	--- Make a pick for third item.
	-- @pick
	-- @function Expect:third
	third = 3,

	--- Make a pick for fourth item.
	-- @pick
	-- @function Expect:fourth
	fourth = 4,

	--- Make a pick for fifth item.
	-- @pick
	-- @function Expect:fifth
	fifth = 5,

	--- Make a pick for sixth item.
	-- @pick
	-- @function Expect:sixth
	sixth = 6,

	--- Make a pick for seventh item.
	-- @pick
	-- @function Expect:seventh
	seventh = 7,

	--- Make a pick for eight item.
	-- @pick
	-- @function Expect:eight
	eight = 8,

	--- Make a pick for ninth item.
	-- @pick
	-- @function Expect:ninth
	ninth = 9,

	--- Make a pick for tenth item.
	-- @pick
	-- @function Expect:tenth
	tenth = 10,

	--- Make a pick for eleventh item.
	-- @pick
	-- @function Expect:eleventh
	eleventh = 11,

	--- Make a pick for twelfth item.
	-- @pick
	-- @function Expect:twelfth
	twelfth = 12
}

-- loop over the list of picks and create the functions
for name,val in pairs( picks ) do
	assert( not Expect[name], name )
	Expect[name] = _makePickProcess( val )
end

--- Make a delayed process for the transform functions.
-- This is a private function that will create a function with a closure.
-- The delayed function comes from the provided definition.
-- @raise on wrong arguments
-- @local
-- @delayed
-- @tparam function proc to adjust the process
-- @treturn function
local function _makeTransformProcess( proc )
	-- not public interface, but will verify if the defs are reasonable
	libUtil.checkType( '_makeTransformProcess', 1, proc, 'function', false )
	local f = function( self )
		self:addProcess( proc )
		return self
	end
	return f
end

-- @var table of definitions for the transforms
-- Format is ''name'' = { ''function'', { ''aliases, ... }
local transforms = {
	--- Make a transform to get the argument type.
	-- @transform
	-- @function Expect:asType
	-- @nick Expect:type
	asType = {
		function( val )
			return type( val )
		end,
		{ 'type' } },

	--- Make a transform to get the string as upper case.
	-- @transform
	-- @function Expect:asUpper
	-- @nick Expect:upper
	-- @nick Expect:asUC
	-- @nick Expect:uc
	asUpper = {
		function( str )
			return string.upper( str )
		end,
		{ 'upper', 'asUC', 'uc' } },

	--- Make a transform to get the string as lower case.
	-- @transform
	-- @function Expect:asLower
	-- @nick Expect:lower
	-- @nick Expect:asLC
	-- @nick Expect:lc
	asLower = {
		function( str )
			return string.lower( str )
		end,
		{ 'lower', 'asLC', 'lc' } },

	--- Make a transform to get the string with first char as upper case.
	-- @transform
	-- @function Expect:asUpperFirst
	-- @nick Expect:upperfirst
	-- @nick Expect:asUCFirst
	-- @nick Expect:asUCfirst
	-- @nick Expect:ucfirst
	asUpperFirst = {
		function( str )
			return string.upper( string.sub( str, 1, 1 ) )..string.sub( str, 2 )
		end,
		{ 'upperfirst', 'asUCFirst', 'asUCfirst', 'ucfirst' } },

	--- Make a transform to get the string with first char as lower case.
	-- @transform
	-- @function Expect:asLowerFirst
	-- @nick Expect:lowerfirst
	-- @nick Expect:asLCFirst
	-- @nick Expect:asLCfirst
	-- @nick Expect:lcfirst
	asLowerFirst = {
		function( str )
			return string.lower( string.sub( str, 1, 1 ) )..string.sub( str, 2 )
		end,
		{ 'lowerfirst', 'asLCFirst', 'asLCfirst', 'lcfirst' } },

	--- Make a transform to get the string reversed.
	-- @transform
	-- @function Expect:asReverse
	-- @nick Expect:reverse
	asReverse = {
		function( str )
			return string.reverse( str )
		end,
		{ 'reverse' } },

	--- Make a transform to get the ustring as upper case.
	-- @transform
	-- @function Expect:asUUpper
	-- @nick Expect:uupper
	-- @nick Expect:asUUC
	-- @nick Expect:uuc
	asUUpper = {
		function( str )
			return mw.ustring.upper( str )
		end,
		{ 'uupper', 'asUUC', 'uuc' } },

	--- Make a transform to get the ustring as lower case.
	-- @transform
	-- @function Expect:asULower
	-- @nick Expect:ulower
	-- @nick Expect:asULC
	-- @nick Expect:ulc
	asULower = {
		function( str )
			return mw.ustring.lower( str )
		end,
		{ 'ulower', 'asULC', 'ulc' } },

	--- Make a transform to get the ustring with first code point as upper case.
	-- @transform
	-- @function Expect:asUUpperFirst
	-- @nick Expect:uupperfirst
	-- @nick Expect:asUUCFirst
	-- @nick Expect:asUUCfirst
	-- @nick Expect:uucfirst
	asUUpperFirst = {
		function( str )
			return mw.ustring.upper( mw.ustring.sub( str, 1, 1 ) )..mw.ustring.sub( str, 2 )
		end,
		{ 'uupperfirst', 'asUUCFirst', 'asUUCfirst', 'uucfirst' } },

	--- Make a transform to get the ustring with first code point as lower case.
	-- @transform
	-- @function Expect:asULowerFirst
	-- @nick Expect:ulowerfirst
	-- @nick Expect:asULCFirst
	-- @nick Expect:asULCfirst
	-- @nick Expect:ulcfirst
	asULowerFirst = {
		function( str )
			return mw.ustring.lower( mw.ustring.sub( str, 1, 1 ) )..mw.ustring.sub( str, 2 )
		end,
		{ 'ulowerfirst', 'asULCFirst', 'asULCfirst', 'ulcfirst' } },

	--- Make a transform to get the ustring as Normalized Form "C".
	-- @transform
	-- @function Expect:asUNFC
	-- @nick Expect:unfc
	-- @nick Expect:uNFC
	-- @nick Expect:nfc
	asUNFC = {
		function( str )
			return mw.ustring.toNFC( str )
		end,
		{ 'unfc', 'uNFC', 'nfc' } },

	--- Make a transform to get the ustring as Normalized Form "D".
	-- @transform
	-- @function Expect:asUNFD
	-- @nick Expect:unfd
	-- @nick Expect:uNFD
	-- @nick Expect:nfd
	asUNFD = {
		function( str )
			return mw.ustring.toNFD( str )
		end,
		{ 'unfd', 'uNFD', 'nfd' } },

	--- Make a transform to get the string as number.
	-- @transform
	-- @function Expect:asNumber
	-- @nick Expect:number
	-- @nick Expect:asNum
	-- @nick Expect:num
	asNumber = {
		function( str )
			return tonumber( str )
		end,
		{ 'number', 'asNum', 'num' } },

	--- Make a transform to get the number as string.
	-- @transform
	-- @function Expect:asString
	-- @nick Expect:string
	-- @nick Expect:asStr
	-- @nick Expect:str
	asString = {
		function( num )
			return tostring( num )
		end,
		{ 'string', 'asStr', 'str' } },

	--- Make a transform to get the next lower number.
	-- @transform
	-- @function Expect:asFloor
	-- @nick Expect:floor
	asFloor = {
		function( num )
			return math.floor( num )
		end,
		{ 'floor' } },

	--- Make a transform to get the next higher number.
	-- @transform
	-- @function Expect:asCeil
	-- @nick Expect:ceil
	asCeil = {
		function( num )
			return math.ceil( num )
		end,
		{ 'ceil' } },

	--- Make a transform to get the rounded number.
	-- @transform
	-- @function Expect:asRound
	-- @nick Expect:round
	asRound = {
		function( num )
			return num % 1 >= 0.5 and math.ceil( num ) or math.floor( num )
		end,
		{ 'round' } },

	--- Make a transform to get the integer part of the number.
	-- @transform
	-- @function Expect:asInteger
	-- @nick Expect:integer
	-- @nick Expect:asInt
	-- @nick Expect:int
	asInteger = {
		function( num )
			return num < 0 and math.ceil( num ) or math.floor( num )
		end,
		{ 'integer', 'asInt', 'int' } },

	--- Make a transform to get the fraction part of the number.
	-- @transform
	-- @function Expect:asFraction
	-- @nick Expect:fraction
	-- @nick Expect:asFrac
	-- @nick Expect:frac
	asFraction = {
		function( num )
			return num - ( num < 0 and math.ceil( num ) or math.floor( num ) )
		end,
		{ 'fraction', 'asFrac', 'frac' } },
}

-- loop over the list of transforms and create the functions
for name,lst in pairs( transforms ) do
	assert( not Expect[name], name )
	local proc = lst[1]
	Expect[name] = _makeTransformProcess( proc )
	for _,alias in ipairs( lst[2] ) do
		assert( not Expect[alias], alias )
		Expect[alias] = Expect[name]
	end
end

--- Make a delayed process for the condition functions.
-- This is a private function that will create a function with a closure.
-- The delayed function comes from the provided definition.
-- @raise on wrong arguments
-- @local
-- @delayed
-- @tparam function proc to adjust the process
-- @tparam any ... values to compare
-- @treturn function
local function _makeConditionProcess( proc, ... )
	-- not public interface, but will verify if the defs are reasonable
	libUtil.checkType( '_makeConditionProcess', 1, proc, 'function', false )
	local f = function( self, ... )
		local keep = { ... }
		local g = function( ... )
			local a = keep
			local b = { ... }
			if #a ~= #b then
				return false
			end
			for i=1,#a do
				if not proc( a[i], b[i] ) then
					return false
				end
			end
			return true
		end
		self:addProcess( g )
		return self
	end
	return f
end

-- @var table of definitions for the conditions
-- Format is ''name'' = { ''function'', { ''aliases, ... }
local conditions = {
	--- Make a comparison to check equality.
	-- @condition
	-- @function toBeEqual
	-- @nick Expect:equal
	-- @nick Expect:isEqual
	-- @nick Expect:ifEqual
	toBeEqual = {
		function ( a, b )
			return a == b
		end,
		{ 'equal', 'isEqual', 'ifEqual' } },

	--- Make a comparison to check boolean equality.
	-- @condition
	-- @function toBeBooleanEqual
	-- @nick Expect:booleanequal
	-- @nick Expect:isBooleanEqual
	-- @nick Expect:ifBooleanEqual
	toBeBooleanEqual = {
		function ( a, b )
			return ( not not a ) == ( not not b )
		end,
		{ 'booleanequal', 'isBooleanEqual', 'ifBooleanEqual' } },

	--- Make a comparison to check strict equality.
	-- @condition
	-- @function toBeStrictEqual
	-- @nick Expect:strictequal
	-- @nick Expect:isStrictEqual
	-- @nick Expect:ifStrictEqual
	toBeStrictEqual = {
		function ( a, b )
			return a == b and type( a ) == type( b )
		end,
		{ 'strictequal', 'isStrictEqual', 'ifStrictEqual' } },

	--- Make a comparison to check similarity.
	-- @condition
	-- @function toBeSame
	-- @nick Expect:same
	-- @nick Expect:isSame
	-- @nick Expect:ifSame
	toBeSame = {
		function ( a, b )
			if ( type( a ) == type( b ) ) then
				return a == b
			elseif type( a ) == 'string' and type( b ) == 'number' then
				return a == tostring( b )
			elseif type( a ) == 'number' and type( b ) == 'string' then
				return a == tonumber( b )
			else
				return a == b
			end
		end,
		{ 'same', 'isSame', 'ifSame' } },

	--- Make a comparison to check deep equality.
	-- @condition
	-- @function toBeDeepEqual
	-- @nick Expect:deepequal
	-- @nick Expect:isDeepEqual
	-- @nick Expect:ifDeepEqual
	toBeDeepEqual = {
		function ( a, b )
			return util.deepEqual( b, a )
		end,
		{ 'deepequal', 'isDeepEqual', 'ifDeepEqual' } },

	--- Make a comparison to check if first is contained in second.
	-- @condition
	-- @function toBeContained
	-- @nick Expect:contained
	-- @nick Expect:isContained
	-- @nick Expect:ifContained
	toBeContained = {
		function ( a, b )
			return util.contains( b, a )
		end,
		{ 'contained', 'isContained', 'ifContained' } },

	--- Make a comparison to check if first is strict lesser than second.
	-- @condition
	-- @function toBeLesserThan
	-- @nick Expect:lesser
	-- @nick Expect:lt
	-- @nick Expect:toBeLesser
	-- @nick Expect:toBeLT
	-- @nick Expect:isLesser
	-- @nick Expect:isLT
	-- @nick Expect:ifLesser
	-- @nick Expect:ifLt
	toBeLesserThan = {
		function ( a, b )
			return a < b
		end,
		{
			'lesser', 'lt',
			'toBeLesser', 'toBeLT',
			'isLesser', 'isLT',
			'ifLesser', 'ifLT' }
		},

	--- Make a comparison to check if first is strict greater than second.
	-- @condition
	-- @function toBeGreaterThan
	-- @nick Expect:greater
	-- @nick Expect:gt
	-- @nick Expect:toBeGreater
	-- @nick Expect:toBeGT
	-- @nick Expect:isGreater
	-- @nick Expect:isGT
	-- @nick Expect:ifGreater
	-- @nick Expect:ifGt
	toBeGreaterThan = {
		function ( a, b )
			return a > b
		end,
		{
			'greater', 'gt',
			'toBeGreater', 'toBeGT',
			'isGreater', 'isGT',
			'ifGreater', 'ifGT' }
		},

	--- Make a comparison to check if first is lesser or equal than second.
	-- @condition
	-- @function toBeLesserOrEqual
	-- @nick Expect:lesserOrEqual
	-- @nick Expect:le
	-- @nick Expect:toBeLE
	-- @nick Expect:isLesserOrEqual
	-- @nick Expect:isLE
	-- @nick Expect:ifLesserOrEqual
	-- @nick Expect:ifLE
	toBeLesserOrEqual = {
		function ( a, b )
			return a <= b
		end,
		{
			'lesserOrEqual', 'le',
			'toBeLE',
			'isLesserOrEqual', 'isLE',
			'ifLesserOrEqual', 'ifLE' }
		},

	--- Make a comparison to check if first is strict greater or equal than second.
	-- @condition
	-- @function toBeGreaterOrEqual
	-- @nick Expect:greaterOrEqual
	-- @nick Expect:ge
	-- @nick Expect:toBeGE
	-- @nick Expect:isGreaterOrEqual
	-- @nick Expect:isGE
	-- @nick Expect:ifGreaterOrEqual
	-- @nick Expect:ifGE
	toBeGreaterOrEqual = {
		function ( a, b )
			return a >= b
		end,
		{
			'greaterOrEqual', 'ge',
			'toBeGE',
			'isGreaterOrEqual', 'isGE',
			'ifGreaterOrEqual', 'ifGE' }
		},

	--- Make a comparison to check if first is a match in second.
	-- @condition
	-- @function toBeMatch
	-- @nick Expect:match
	-- @nick Expect:isMatch
	-- @nick Expect:ifMatch
	toBeMatch = {
		function ( a, b )
			return string.match( b, a ) or false
		end,
		{ 'match', 'isMatch', 'ifMatch' } },

	--- Make a comparison to check if first is an Unicode match in second.
	-- @condition
	-- @function toBeUMatch
	-- @nick Expect:umatch
	-- @nick Expect:isUMatch
	-- @nick Expect:ifUMatch
	toBeUMatch = {
		function ( a, b )
			return mw.ustring.match( b, a ) or false
		end,
		{ 'umatch', 'isUMatch', 'ifUMatch' } },
}

-- loop over the list of conditions and create the functions
for name,lst in pairs( conditions ) do
	assert( not Expect[name], name )
	local proc = lst[1]
	Expect[name] = _makeConditionProcess( proc )
	for _,alias in ipairs( lst[2] ) do
		assert( not Expect[alias], alias )
		Expect[alias] = Expect[name]
	end
end

-- Return the final class.
return Expect
