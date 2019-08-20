<?php

namespace Expect;

use MediaWiki\Logger\LoggerFactory;

/**
 * Hook handlers for the Expect extension
 *
 * @ingroup Extensions
 */

class Hooks {

	/**
	 * Setup for the extension
	 */
	public static function onExtensionSetup() {
		global $wgDebugComments;

		// turn on comments while in development
		$wgDebugComments = true;
	}

	/**
	 * External Lua library paths for Scribunto
	 *
	 * @param any $engine to be used for the call
	 * @param array &$extraLibraryPaths additional libs
	 * @return bool
	 */
	public static function onRegisterScribuntoExternalLibraryPaths(
		$engine,
		array &$extraLibraryPaths
	) {
		if ( $engine !== 'lua' ) {
			return true;
		}

		// Path containing pure Lua libraries that don't need to interact with PHP
		$extraLibraryPaths[] = __DIR__ . '/LuaLibrary/lua/pure';

		return true;
	}

	/**
	 * Extra Lua libraries for Scribunto
	 *
	 * @param any $engine to be used for the call
	 * @param array &$extraLibraries additional libs
	 * @return bool
	 */
	public static function onRegisterScribuntoLibraries(
		$engine,
		array &$extraLibraries
	) {
		if ( $engine !== 'lua' ) {
			return true;
		}

		$extraLibraries['expect'] = [
			'class' => '\Expect\LuaLibExpect',
			'deferLoad' => false
		];

		return true;
	}
}