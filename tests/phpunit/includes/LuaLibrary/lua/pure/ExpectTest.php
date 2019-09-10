<?php

declare( strict_types = 1 );

namespace Expect\Test;

use Scribunto_LuaEngineTestBase;

/**
 * @group Expect
 *
 * @license GPL-2.0-or-later
 *
 * @author John Erling Blad < jeblad@gmail.com >
 */
class ExpectTest extends Scribunto_LuaEngineTestBase {

	protected static $moduleName = 'ExpectTest';

	/**
	 * @slowThreshold 1000
	 * @see Scribunto_LuaEngineTestBase::getTestModules()
	 */
	protected function getTestModules(): array {
		return parent::getTestModules() + [
			'ExpectTest' => __DIR__ . '/ExpectTest.lua'
		];
	}
}
