package Code.Geometry
{
	import Code.Maths.*;
	import Code.System.*;
	import Code.*;
	
	public class AABB implements IAABB
	{
		private var m_centre:Vector2;
		private var m_halfExtents:Vector2;

		/// <summary>
		/// 
		/// </summary>
		/// <param name="centre"></param>
		/// <param name="halfExtents"></param>
		public function AABB(centre:Vector2=null, halfExtents:Vector2=null)
		{
			Initialise( centre, halfExtents );	
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Initialise(centre:Vector2, halfExtents:Vector2):void
		{
			m_Centre = centre;
			m_halfExtents = halfExtents;
			
			if ( m_halfExtents!=null )
			{
				Assert( m_halfExtents.m_x>=0&&m_halfExtents.m_y>=0, "AABB.Initialise(): cannot have negative half extents!" );
			}
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Centre( ):Vector2
		{
			return m_centre;
		}

		/// <summary>
		/// 
		/// </summary>	
		public function set m_Centre( p:Vector2 ):void
		{
			m_centre = p;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_HalfExtents():Vector2
		{
			return m_halfExtents;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_BottomLeft():Vector2
		{
			return m_centre.Add( new Vector2(-m_halfExtents.m_x, m_halfExtents.m_y) );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_BottomRight():Vector2
		{
			return m_centre.Add( new Vector2(m_halfExtents.m_x, m_halfExtents.m_y) );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_TopLeft():Vector2
		{
			return m_centre.Add( new Vector2(-m_halfExtents.m_x, -m_halfExtents.m_y) );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_TopRight():Vector2
		{
			return m_centre.Add( new Vector2(m_halfExtents.m_x, -m_halfExtents.m_y) );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Min():Vector2
		{
			return m_TopLeft;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Max():Vector2
		{
			return m_BottomRight;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function MinInto( v:Vector2 ):Vector2
		{
			v.CloneInto( m_centre );
			v.SubFrom( m_halfExtents );
			
			return v;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Enlarge( amount:Number ):void
		{
			m_halfExtents.m_x += amount;
			m_halfExtents.m_y += amount;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function EnlargeY( amount:Number ):void
		{
			m_halfExtents.m_y += amount;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="a"></param>
		/// <param name="b"></param>
		/// <returns></returns>
		static public function Overlap(a:IAABB, b:IAABB):Boolean
		{
			var d:Vector2 = b.m_Centre.Sub(a.m_Centre).m_Abs.Sub(a.m_HalfExtents.Add(b.m_HalfExtents));
			return d.m_x < 0 && d.m_y < 0;
		}
		
		/**
		 */
		static public function AabbWithinFlags( candidate:AABB, container:AABB ):uint
		{
			var flags:uint = 0;
			var delta:Vector2 = candidate.m_Centre.Sub( container.m_Centre );
			
			// check -x
			if ( delta.m_x-candidate.m_HalfExtents.m_x<-container.m_HalfExtents.m_x )
			{
				flags |= OverlapFlags.kMinusX;
			}
			
			// check -y
			if ( delta.m_y-candidate.m_HalfExtents.m_y<-container.m_HalfExtents.m_y )
			{
				flags |= OverlapFlags.kMinusY;
			}
			
			// check +x
			if ( delta.m_x+candidate.m_HalfExtents.m_x>container.m_HalfExtents.m_x )
			{
				flags |= OverlapFlags.kPlusX;
			}
			
			// check +y
			if ( delta.m_y+candidate.m_HalfExtents.m_y>container.m_HalfExtents.m_y )
			{
				flags |= OverlapFlags.kPlusY;
			}
			
			return flags;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Within( p:Vector2 ):Boolean
		{
			var d:Vector2 = p.Sub( m_centre ).m_Abs;
			return d.m_x<m_halfExtents.m_x&&d.m_y<m_halfExtents.m_y;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function GetRandomPointWithin( scaleX:Number=1, scaleY:Number=1 ):Vector2
		{
			var point:Vector2 = new Vector2( );
			point.CloneInto( m_centre );
			point.m_x += Scalar.RandBetween( -m_halfExtents.m_x, m_halfExtents.m_x )*scaleX;
			point.m_y += Scalar.RandBetween( -m_halfExtents.m_y, m_halfExtents.m_y )*scaleY;
			return point;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function ClampInto(candidate:Vector2):Vector2
		{
			return candidate.SubFrom(m_Centre).ClampInto(m_Max, m_Min).AddTo(m_Centre);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function UpdateFrom( centre:Vector2, halfExtents:Vector2 ):void
		{
			m_centre = centre;
			m_halfExtents = halfExtents;
		}
	}
}
