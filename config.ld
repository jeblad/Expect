project = 'Expect'
title = 'Expect documentation'
description = 'Assertion framework for Lua embedded within Mediawiki.'
full_description = [[The [Expect extension](https://mediawiki.org/wiki/Extension:Expect) is a
project to make extended [assertions](https://en.wikipedia.org/wiki/Assertion_(software_development))
available in [Lua-scripts](https://en.wikipedia.org/wiki/Lua_programming_language) for the
[Scribunto extension](https://mediawiki.org/wiki/Extension:Scribunto), which is used by
[Wikipedia](https://en.wikipedia.org/wiki/Wikipedia) and other sites. Within Wikipedia Lua is used
to implement advanced templates with [Lua modules](https://en.wikipedia.org/wiki/Help:Lua), and the
same solution is used on a lot of other websites and projects.
Expect assertions is a kind of assertion definition language, where a compute graph is defined and
later executed with specific values. When a compute graph evaluates to false, the code has failed
the premises, and execution has reached an error condition. It will then throw an error and stop
further execution.]]
use_markdown_titles = true
format = 'markdown'
dir = 'docs'
--output = 'mw.expect'
-- all = true

readme = {
	'topics/expect.md',
	'topics/vagrant.md',
	'topics/databases.md',
	-- 'topics/delayed.md',
	-- 'topics/hello-world.md',
	-- 'topics/translatewiki.md',
	'topics/vscode.md',
	'CONTRIBUTING.md',
	'CODE_OF_CONDUCT.md',
	'SECURITY.md',
	'TRANSLATING.md'
}

examples = {
	'examples'
}

-- new_type( 'macro', 'Macros' )
custom_tags = {
	{
		'nick',
		title = 'Nicknames',
		hidden = false,
		format = nil
	},
	{
		'delayed',
		title = 'Process',
		hidden = false,
		format = function()
			return '<a href="topics/expect.md.html#delayed">delayed</a>'
		end
	},
	{
		'pick',
		title = 'Process',
		hidden = false,
		format = function()
			return '<a href="topics/expect.md.html#pick">pick</a>'
		end
	},
	{
		'transform',
		title='Process',
		hidden = false,
		format = function()
			return '<a href="topics/expect.md.html#transform">transform</a>'
		end
	},
	{
		'condition',
		title='Process',
		hidden = false,
		format = function()
			return '<a href="topics/expect.md.html#condition">condition</a>'
		end
	}
}

custom_display_name_handler = function(item, default_handler)
	-- show a label beside functions tagged with @delayed.
	if item.type == 'lfunction' or item.type == 'function' then
		return item.name .. ' ()'
	end
	return default_handler(item)
end
