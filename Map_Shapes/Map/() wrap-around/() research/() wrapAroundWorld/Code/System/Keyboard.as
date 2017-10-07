package Code.System
{
	import flash.display.Stage;
	import flash.events.KeyboardEvent;
	
	public class Keyboard
	{
		private const kNumKeyCodes:int = 255;
		
		private var m_liveKeyState:Vector.<Boolean>;
		private var m_keyState:Vector.<Boolean>;
		private var m_lastKeyState:Vector.<Boolean>;
		
		private var m_onKeyUp:Function;
		
		/// <summary>
		/// 
		/// </summary>	
		public function Keyboard( stage:Stage, onKeyUp:Function=null )
		{
			m_onKeyUp = onKeyUp;
			
			// add handlers
			stage.addEventListener( KeyboardEvent.KEY_DOWN, OnKeyDown, false, 0, true );
			stage.addEventListener( KeyboardEvent.KEY_UP, OnKeyUp, false, 0, true );
			
			m_keyState = new Vector.<Boolean>( kNumKeyCodes );
			m_lastKeyState = new Vector.<Boolean>( kNumKeyCodes );
			m_liveKeyState = new Vector.<Boolean>( kNumKeyCodes );
			
			// clear state
			for ( var i:int = 0; i<kNumKeyCodes; i++ )
			{
				m_keyState[i] = false;
				m_lastKeyState[i] = false;
				m_liveKeyState[i] = false;
			}
		}
		
		/// <summary>
		/// 
		/// </summary>
		public function Update( ):void
		{
			for ( var i:int = 0; i<kNumKeyCodes; i++ )
			{
				m_lastKeyState[i] = m_keyState[i];
				m_keyState[i] = m_liveKeyState[i];
			}
		}
		
		/// <summary>
		/// 
		/// </summary>	
		private function OnKeyDown( e:KeyboardEvent ):void
		{
			m_liveKeyState[e.keyCode] = true;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		private function OnKeyUp( e:KeyboardEvent ):void
		{
			m_liveKeyState[e.keyCode] = false;
			
			if ( m_onKeyUp != null )
			{
				m_onKeyUp( e, this );
			}
		}

		/// <summary>
		/// 
		/// </summary>	
		public function IsKeyDown( key:int ):Boolean
		{
			Assert( key>=0&&key<=kNumKeyCodes, "Keyboard.IsKeyDown(): invalid key!" );
			return m_keyState[key];
		}
		
		/// <summary>
		/// Is key transitioning from up to down?
		/// </summary>
		public function IsKeyDownTransition( key:int ):Boolean
		{
			Assert( key>=0&&key<=kNumKeyCodes, "Keyboard.IsKeyDownTransition(): invalid key!" );
			return !m_lastKeyState[key] && m_keyState[key];
		}
		
		/// <summary>
		/// Is key transitioning from up to down?
		/// </summary>
		public function IsKeyUpTransition( key:int ):Boolean
		{
			Assert( key>=0&&key<=kNumKeyCodes, "Keyboard.IsKeyDownTransition(): invalid key!" );
			return m_lastKeyState[key] && !m_keyState[key];
		}
	}
}
