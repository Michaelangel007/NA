package Code.System
{
	public function Assert(expression:Boolean, message:String="") : void
	{
		if ( !expression )
		{
			throw new Error( "Assertion failed: " + message );
		}
	}
}
