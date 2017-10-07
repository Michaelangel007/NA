package Code.System
{
	import flash.net.*;
	
	/**
	 * A collection of useful functions
	 */
	public class Utils
	{
		/**
		 Open the given link in the browser
		*/
		static public function OpenLink( url:String ):void
		{	
			var targetURL:URLRequest = new URLRequest(url);
			navigateToURL(targetURL);
		}
		
		/**
		 * Clone the given anonymous object into a concrete type.
		 * 
		 * @param source The anonymous object
		 * @param targetType The type to clone into
		 * @return Object of the given type
		 */
		static public function CloneInto( source:Object, targetType:Class ):*
		{
			Assert( source!=null );
			var data:* = new targetType( );
			
			for ( var prop:Object in source )
			{
				Assert( data.hasOwnProperty( prop ), "Utils.CloneInto(): supplied type didn't have required property " + prop );
				
				data[prop] = source[prop];
			}
			
			return data;
		}	
	}
}
