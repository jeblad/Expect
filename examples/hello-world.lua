-- Load the lib
local expect = require 'expect'

-- Create a few compute graphs
local expectString = expect:create():asType():toBeEqual( 'string' )
local expectNoColon = expect:create():toBeUMatch( '^[^:]*$' )

-- Create the exported hash
local p = {}

-- Add a semi-private function
function p._hello( name )
	-- Call the compute graphs
	expectString( name )
	expectNoColon( name )

	-- Should be safe to do whatever now
	return mw.ustring.format( 'Hi there %s!', name )
end

-- Add a public function
function p.hello( frame )
	return p._hello( frame.args['name'] )
end

-- Return the exported hash
return p
