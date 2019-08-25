--- Class for Expect.
-- This class follows the pattern from
-- [Lua classes](../topics/lua-classes.md.html).
-- @classmod Expect

-- @var class var for lib.
local Expect = {}

local libUtil = require 'libraryUtil'
local expUtil = require '_expect.util'

--- Global assert soft fail.
-- This toggles soft errors, thus allows easy testing of the compute graph.
-- @ tvar boolean softFail
Expect.softFail = nil

-- global bypass eval
-- This toggle process execution, thus speeds up evaluation of the compute graph.
-- @ tvar boolean bypassEval
Expect.bypassEval = nil

-- global argument type checks
-- This toggle type assertions, thus speeds up creation of the compute graph.
-- @ tvar boolean typeCheck
Expect.typeCheck = nil

--- Lookup of missing class members.
-- @raise on wrong argument type, unless turned off by @expect.typecheck.
-- @tparam string key lookup of member
-- @return any
function Expect:__index( key ) -- luacheck: no self
	if Expect.typeCheck then
		libUtil.checkType( 'Expect:__index', 1, key, 'string', false )
	end
	return Expect[key]
end

--- Get a clone or create a new instance.
-- @function Expect:__call
-- @tparam vararg ... conditionally passed to create
-- @treturn self
function Expect:__call( ... )
	local instance = rawget( self, 'create' ) and self:create() or self
	return instance:eval( ... )
end

--- Create a new instance.
-- @raise on wrong arguments type, unless turned off by @expect.typecheck.
-- @tparam vararg ... forwarded to `_init()`
-- @treturn self
function Expect:create( ... )
	if Expect.typeCheck then
		for i,v in ipairs( { ... } ) do
			libUtil.checkTypeMulti( 'Expect:create', i, v, { 'string', 'boolean', 'table', 'function' } )
		end
	end
	local meta = rawget( self, 'create' ) and self or getmetatable( self )
	local new = setmetatable( {}, meta )
	return new:_init( ... )
end

--- Initialize a new instance.
-- @raise on wrong arguments
-- @tparam vararg ... set to temporal
-- @treturn self
function Expect:_init( ... )
	self._processes = {}
	self._softFail = Expect.softFail
	for _,v in ipairs( { ... } ) do
		local t = type( v )
		if t == 'string' then
			if not self._name then
				self._name = v
			else
				error( mw.ustring.format( 'Expect:create "%s" called with multiple strings "%s"',
				self._name or 'anonymous', v ) )
			end
		elseif t == 'boolean' then
			if not self._softFail then
				self._softFail = v
			else
				error( mw.ustring.format( 'Expect:create "%s" called with multiple booleans "%s"',
				self._name or 'anonymous', ( v and 'true' or 'false' ) ) )
			end
		elseif t == 'table' then
			self:import( v )
		elseif t == 'function' then
			if not self._onPass then
				self._onPass = { v }
			elseif not self._onFail then
				self._onFail = { v }
			else
				error( mw.ustring.format( 'Expect:create "%s" called with multiple functions',
				self._name or 'anonymous' ) )
			end
		else
			error( mw.ustring.format( 'Expect:create "%s" called with unsupported type "%s"',
				self._name or 'anonymous', type( v ) ) )
		end
	end
	return self
end

--- Test whether instance is tainted
-- @treturn boolean
function Expect:isTainted()
	return self._taint
end

--- Test whether instance has soft fail.
-- @treturn boolean
function Expect:hasSoft()
	return type( self._softFail ) == 'boolean'
end

--- Test whether instance will soft fail
-- @treturn boolean
function Expect:isSoft()
	return self._softFail
end

--- Test whether instance has name.
-- @treturn boolean
function Expect:hasName()
	return type( self._name ) == 'string'
end

--- Get instance name.
-- @treturn string
function Expect:getName()
	return self._name
end

--- Add callback for fail.
-- @tparam function func to call
function Expect:addFail( func )
	self._onFail = self._onFail or {}
	self._taint = true
	table.insert( self._onFail, func )
	return self
end

--- Add report for fail.
-- @tparam table tbl to call
-- @tparam string msg to call
function Expect:addFailReport( tbl, msg )
	self._onFail = self._onFail or {}
	local func = function()
		table.insert( tbl, msg )
	end
	table.insert( self._onFail, func )
	return self
end

--- Add callback for pass.
-- @tparam function func to call
function Expect:addPass( func )
	self._onPass = self._onPass or {}
	self._taint = true
	table.insert( self._onPass, func )
	return self
end

--- Add report for pass.
-- @tparam table tbl to call
-- @tparam string msg to call
function Expect:addPassReport( tbl, msg )
	self._onPass = self._onPass or {}
	local func = function()
		table.insert( tbl, msg )
	end
	table.insert( self._onPass, func )
	return self
end

--- Callback on pass.
-- The callback is evaluated right before @Expect:compare() returns.
-- @tparam table cb to call
function Expect:callbacks( cb )
	if cb then
		for _,v in ipairs( cb ) do
			v( self )
		end
	end
end

--- Import a compute grap.
-- This is scary, and graph will be tainted.
-- @raise on wrong argument types, unless turned off by @expect.typecheck.
-- @tparam table procs for the graph
-- @treturn self
function Expect:import( procs )
	self._taint = true
	for _,v in ipairs( procs ) do
		if Expect.typeCheck then
			libUtil.checkType( 'Expect:import', 1, v, 'function', false )
		end
		table.insert( self._processes, v )
	end
	return self
end

--- Add a process function
-- @raise on wrong argument type, unless turned off by @expect.typecheck.
-- @tparam function proc to be evaluated
-- @tparam[hold=nil] nil|boolean hold the tainting
function Expect:addProcess( proc, hold )
	if not hold then
		self._taint = true
	end
	if Expect.typeCheck then
		libUtil.checkType( 'Expect:addProcess', 1, proc, 'function', false )
	end
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
			self:callbacks( self._onFail )
			return false, self._name
		end
		table.remove( tmp, 1 )
	end
	for _,v in ipairs( tmp ) do
		if not v then
			self:callbacks( self._onFail )
			return false, self._name
		end
	end
	self:callbacks( self._onPass )
	return true, self._name
end

--- Eval given values
-- @tparam varargs ... any used as arguments
-- @return list of any
function Expect:eval( ... )
	if Expect.bypassEval then
		return true
	end
	local result = { self:compare( ... ) }
	if not result[1] then
		if self._softFail then
			return unpack( result )
		end
		error( result[1], 2 )
	end
	return unpack( result )
end

--- Pick entries
-- @tparam varargs ... any used as indexes
-- @return list of any
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
	self:addProcess( g, true )
	return self
end

--- Filter entries
-- @tparam function func to filter the set
-- @tparam varargs ... arguments passed to func
-- @function filter
-- @return list of any
function Expect:filter( func, ... )
	local keep = { ... }
	local g = function( ... )
		local args = { ... }
		local t = {}
		for i,v in ipairs( args ) do
			if func( i, v, unpack( keep ) ) then
				table.insert( t, v )
			end
		end
		return unpack( t )
	end
	self:addProcess( g, false )
	return self
end

--- Map over entries
-- @tparam function func to map over the set
-- @tparam varargs ... arguments passed to func
-- @function filter
-- @return list of any
function Expect:map( func, ... )
	local keep = { ... }
	local g = function( ... )
		local args = { ... }
		local t = {}
		for i,v in ipairs( args ) do
			table.insert( t, func( i, v, unpack( keep ) ) )
		end
		return unpack( t )
	end
	self:addProcess( g, false )
	return self
end

--- Make a delayed process for specific pick functions.
-- This is a private function that will create a function with a closure.
-- It will create an additional delayed function for the provided definition.
-- @local
-- @delayed
-- @raise on wrong argument type, unless turned off by @expect.typecheck.
-- @tparam number idx of the extracted item
-- @treturn function
local function makePickProcess( idx )
	if Expect.typeCheck then
		-- not public interface, but will verify if the defs are reasonable
		libUtil.checkType( 'makePickProcess', 1, idx, 'number', false )
	end
	local g = function( ... )
		local t = { ... }
		return t[idx]
	end
	local f = function( self )
		self:addProcess( g, true )
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
	Expect[name] = makePickProcess( val )
end

--- Make a delayed process for the transform functions.
-- This is a private function that will create a function with a closure.
-- The delayed function comes from the provided definition.
-- @raise on wrong argument type, unless turned off by @expect.typecheck.
-- @local
-- @delayed
-- @tparam function proc to adjust the process
-- @treturn function
local function makeTransformProcess( proc )
	if Expect.typeCheck then
		-- not public interface, but will verify if the defs are reasonable
		libUtil.checkType( 'makeTransformProcess', 1, proc, 'function', false )
	end
	local f = function( self )
		self:addProcess( proc, true )
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
			return ( num < 0 ) and math.ceil( num ) or math.floor( num )
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
			local val = num - ( ( num < 0 ) and math.ceil( num ) or math.floor( num ) )
			return val
		end,
		{ 'fraction', 'asFrac', 'frac' } },
}

-- loop over the list of transforms and create the functions
for name,lst in pairs( transforms ) do
	assert( not Expect[name], name )
	local proc = lst[1]
	Expect[name] = makeTransformProcess( proc )
	for _,alias in ipairs( lst[2] ) do
		assert( not Expect[alias], alias )
		Expect[alias] = Expect[name]
	end
end

---Broadcast over arguments
-- @local
-- @tparam table a list of arguments
-- @tparam table b list of arguments
-- @tparam function cmp comparator
-- @treturn boolean
local function broadcast( a, b, cmp )
	local lenA = #a
	local lenB = #b
	if lenB == 0 or lenA == 0 then
		return false
	elseif lenB == lenA then
		for i,v in ipairs( a ) do
			if not cmp( v, b[i] ) then
				return false
			end
		end
	elseif lenB < lenA then
		for i,v in ipairs( a ) do
			if not cmp( v, b[((i-1)%lenB)+1] ) then
				return false
			end
		end
	else
		for i,v in ipairs( b ) do
			if not cmp( a[((i-1)%lenA)+1], v ) then
				return false
			end
		end
	end
	return true
end

--- Make a comparison to check if first is within limits of second.
-- @tparam any limit the values must be within
-- @tparam varargs ... any used as indexes
-- @function toBeWithin
-- @return list of any
-- @nick Expect:within
-- @nick Expect:isWithin
-- @nick Expect:ifWithin
function Expect:toBeWithin( limit, ... )
	local function cmp( a, b )
		return math.abs( a -b ) <= limit
	end
	local keep = { ... }
	local g = function( ... )
		local args = { ... }
		return broadcast( keep, args, cmp )
	end
	self:addProcess( g, true )
	return self
end
Expect.within = Expect.toBeWithin
Expect.isWithin = Expect.toBeWithin
Expect.ifWithin = Expect.toBeWithin

--- Make a delayed process for the condition functions.
-- This is a private function that will create a function with a closure.
-- The delayed function comes from the provided definition.
-- Mismatched length will trigger broadcast.
-- @raise on wrong argument type, unless turned off by @expect.typecheck.
-- @local
-- @delayed
-- @tparam function proc to adjust the process
-- @treturn function
local function makeConditionProcess( proc )
	if Expect.typeCheck then
		-- not public interface, but will verify if the defs are reasonable
		libUtil.checkType( 'makeConditionProcess', 1, proc, 'function', false )
	end
	local f = function( self, ... )
		local keep = { ... }
		local g = function( ... )
			local args = { ... }
			return broadcast( keep, args, proc )
		end
		self:addProcess( g, true )
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
			return expUtil.deepEqual( a, b )
		end,
		{ 'deepequal', 'isDeepEqual', 'ifDeepEqual' } },

	--- Make a comparison to check if first is contained in second.
	-- Note that it must be contained at the surface level.
	-- @condition
	-- @function toBeContained
	-- @nick Expect:contained
	-- @nick Expect:isContained
	-- @nick Expect:ifContained
	toBeContained = {
		function ( a, b )
			return expUtil.contains( a, b )
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
			mw.log(mw.ustring.format('a: %s', a))
			mw.log(mw.ustring.format('b: %s', b))
			return mw.ustring.match( b, a ) or false
		end,
		{ 'umatch', 'isUMatch', 'ifUMatch' } },
}

-- loop over the list of conditions and create the functions
for name,lst in pairs( conditions ) do
	assert( not Expect[name], name )
	local proc = lst[1]
	Expect[name] = makeConditionProcess( proc )
	for _,alias in ipairs( lst[2] ) do
		assert( not Expect[alias], alias )
		Expect[alias] = Expect[name]
	end
end

-- Return the final class.
return Expect
