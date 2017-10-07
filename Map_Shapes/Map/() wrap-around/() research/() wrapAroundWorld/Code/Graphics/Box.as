package Code.Graphics
{
	import flash.display.*;
	
	import Code.Geometry.*;
	import Code.Maths.Vector2;
	
	public class Box extends Sprite
	{
		private var m_aabb:AABB;
		
		public function Box( aabb:AABB, colour:uint=0 )
		{
			m_aabb = aabb;
			
			this.graphics.beginFill( colour );
			this.graphics.drawRect( -m_aabb.m_HalfExtents.m_x, -m_aabb.m_HalfExtents.m_y, m_aabb.m_HalfExtents.m_x*2, m_aabb.m_HalfExtents.m_y*2 );
			this.graphics.endFill( );
			
			m_Pos = m_aabb.m_Centre;
		}
		
		/**
		 */
		public function set m_Pos( pos:Vector2 ):void
		{
			m_aabb.m_Centre = pos;
			this.x = pos.m_x;
			this.y = pos.m_y;
		}
		
		/**
		 */
		public function get m_Pos( ):Vector2
		{
			return m_aabb.m_Centre;
		}

		/**
		 */
		public function get m_Aabb( ):AABB
		{
			return m_aabb;
		}
		
		/**
		 */
		public function Within( p:Vector2 ):Boolean
		{
			return m_aabb.Within( p );
		}
	}
}
