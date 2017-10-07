package Code.System
{
	import flash.utils.getTimer;
	import flash.events.*;
	import flash.display.MovieClip;
	
	public class GameLoop extends MovieClip
	{	
		private var m_previousTime:int;
		private var m_started:Boolean;
		
		/// <summary>
		/// 
		/// </summary>	
		public function GameLoop( )
		{
			super();
			m_started = false;
		}

		/// <summary>
		/// 
		/// </summary>	
		private function UpdateInternal( e:Event ):void
		{
			var currentTime:int=getTimer( );
			var difference:int=currentTime-m_previousTime;
			m_previousTime=currentTime;
			
			var secondsElapsed:Number=difference/1000.0;
			
			// call out to anyone listening
			Update( secondsElapsed );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		protected function Update(dt:Number):void
		{
			
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Start( ):void
		{
			if ( !m_started )
			{
				m_previousTime=getTimer( );
				m_started=true;
				this.addEventListener( Event.ENTER_FRAME, UpdateInternal );
			}
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Stop( ):void
		{
			if ( m_started )
			{
				this.removeEventListener( Event.ENTER_FRAME, UpdateInternal );
				m_started=false;
			}
		}
	}
}
