package Code.System
{
	import Code.Maths.Vector2;
	
	public class VectorPool extends Pool
	{
		public function VectorPool( maxObjects:int=0 )
		{
			super( Vector2, maxObjects );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AllocateClone( v:Vector2 ):Vector2
		{
			return Allocate( v.m_x, v.m_y ); 
		}
	}
}
