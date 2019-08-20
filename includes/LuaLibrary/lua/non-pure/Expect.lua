--- Register functions for the expectation framework.
-- @module Expect

-- accesspoints for the boilerplate
local php		-- luacheck: ignore
local options	-- luacheck: ignore

-- pure libs
local libUtil = require 'libraryUtil'

-- @var structure for storage of the lib
local expect = {}

-- Setup framework.
-- This needs a valid environment, for example from getfenv()
-- @tparam table env
-- @tparam table opts
-- @treturn table environment
local function setup( env, opts )
	return env
end

-- @var metatable for the library
local mt = { types = {} }

--- Install the library.
-- This install all dependencies and changes the environment
-- @function mw.expect.__call
-- @tparam table opts for the options
-- @treturn self
function mt:__call( opts ) -- luacheck: no self
	libUtil.checkType( '__call', 1, opts, 'table', true )
	opts = opts or {}

	-- Get the environment for installation of our access points
	-- This is necessary for testing.
	local ret,env = pcall( function() return getfenv( 4 ) end )
	if not ret then
		env = _G
	end

	setup( env, opts )
end

setmetatable( expect, mt )

--- install the module in the global space.
function expect.setupInterface( opts )

	expect.setupInterface = nil
	php = mw_interface
	mw_interface = nil
	options = opts

	mw = mw or {}
	mw.expect = expect
	package.loaded['mw.Expect'] = expect
	expect._IMPLICIT = opts.setup == 'implicit'

end

-- Return the final library
return expect
