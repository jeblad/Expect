-- Load the lib
local expect = require 'expect'

-- Create a few compute graphs
local expectString = expect:create():asType():toBeEqual( 'string' )
local expectNoColon = expect:create():toBeUMatch( '^[^:]*$' )

-- Create an exported hash
local p = {}

-- Add a function
function p.helloWorld( name )
	-- Call the compute graphs
	expectString( name )
	expectNoColon( name )

	-- Should be safe to do whatever now
	return mw.ustring.format( 'Hi there %s!', name )
end

-- Retrurn the exported hash
return p
