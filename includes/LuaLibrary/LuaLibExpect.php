<?php

namespace Expect;

use Scribunto_LuaLibraryBase;

/**
 * Registers our lua modules to Scribunto
 *
 * @ingroup Extensions
 */

class LuaLibExpect extends Scribunto_LuaLibraryBase {

	/**
	 * Register the library
	 *
	 * @return array
	 */
	public function register() {

		return $this->getEngine()->registerInterface(
			__DIR__ . '/lua/non-pure/Expect.lua',
			[ 'addResourceLoaderModules' => [ $this, 'addResourceLoaderModules' ] ]
		);
	}

	/**
	 * Allows Lua to dynamically add the RL modules required for Expect.
	 */
	public function addResourceLoaderModules() {
		$this->getParser()->getOutput()->addModuleStyles( 'ext.expect.report' );
	}

}
