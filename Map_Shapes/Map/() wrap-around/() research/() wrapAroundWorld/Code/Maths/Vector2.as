package Code.Maths
{
	import flash.geom.Point;
	import Code.Constants;
	
	/**
	 * ...
	 * @author me
	 */
	public class Vector2
	{
		public var m_x:Number;
		public var m_y:Number;
		
		/// <summary>
		/// 
		/// </summary>	
		public function Vector2( x:Number = 0, y:Number = 0)
		{
			Initialise( x, y );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Initialise( x:Number=0, y:Number=0 ):void
		{
			m_x = x;
			m_y = y;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Add( v : Vector2 ) : Vector2
		{
			return new Vector2(m_x + v.m_x, m_y + v.m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddTo( v:Vector2 ):Vector2
		{
			m_x += v.m_x;
			m_y += v.m_y;
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubFrom( v:Vector2 ):Vector2
		{
			m_x -= v.m_x;
			m_y -= v.m_y;
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubScalar( s:Number ):Vector2
		{
			return new Vector2( m_x-s, m_y-s );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddScalar( s:Number ):Vector2
		{
			return new Vector2( m_x+s, m_y+s );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubScalarFrom( s:Number ):Vector2
		{
			m_x -= s;
			m_y -= s;
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddScalarTo( s:Number ):Vector2
		{
			m_x += s;
			m_y += s;
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddX( x : Number ) : Vector2
		{
			return new Vector2(m_x + x, m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddY( y : Number ) : Vector2
		{
			return new Vector2(m_x, m_y+y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubX( x:Number ) : Vector2
		{
			return new Vector2(m_x-x, m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubY( y:Number ) : Vector2
		{
			return new Vector2(m_x, m_y-y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddXTo( x : Number ) : Vector2
		{
			m_x += x;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AddYTo( y : Number ) : Vector2
		{
			m_y += y;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubXFrom( x:Number ) : Vector2
		{
			m_x -= x;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function SubYFrom( y:Number ) : Vector2
		{
			m_y -= y;
			return this;
		}
		
		
		/// <summary>
		/// 
		/// </summary>	
		public function Sub( v : Vector2 ) : Vector2
		{
			return new Vector2(m_x - v.m_x, m_y - v.m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Mul( v : Vector2 ) : Vector2
		{
			return new Vector2(m_x * v.m_x, m_y * v.m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>
		public function MulTo( v:Vector2 ):Vector2
		{
			m_x *= v.m_x;
			m_y *= v.m_y;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Div( v : Vector2 ) : Vector2
		{
			return new Vector2(m_x / v.m_x, m_y / v.m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function MulScalar( s : Number ) : Vector2
		{
			return new Vector2(m_x * s, m_y * s);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function MulScalarTo( s : Number ) : Vector2
		{
			m_x *= s;
			m_y *= s;
			
			return this;
		}
		
		/// <summary>
		/// Multiples v by s and then adds to the current vector
		/// </summary>	
		public function MulAddScalarTo( v:Vector2, s:Number ):Vector2
		{
			m_x += v.m_x*s;
			m_y += v.m_y*s;
			
			return this;
		}
		
		/// <summary>
		/// Multiples v by s and then subtracts from the current vector
		/// </summary>	
		public function MulSubScalarTo( v:Vector2, s:Number ):Vector2
		{
			m_x -= v.m_x*s;
			m_y -= v.m_y*s;
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Dot( v : Vector2 ) : Number
		{
			return m_x * v.m_x + m_y * v.m_y;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_LenSqr() : Number
		{
			return Dot(this);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Len() : Number
		{
			return Math.sqrt( m_LenSqr );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Abs() : Vector2
		{
			return new Vector2( Math.abs( m_x ), Math.abs( m_y ) );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Unit() : Vector2
		{
			var invLen : Number = 1.0 / m_Len;
			return MulScalar(invLen);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Floor():Vector2
		{
			return new Vector2(Math.floor(m_x), Math.floor(m_y));
		}
		
		/**
		 */
		public function get m_Annon( ):Object
		{
			return {m_x:m_x, m_y:m_y};
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Clamp(min : Vector2, max : Vector2):Vector2
		{
			return new 	Vector2
						(
							Math.max( Math.min(m_x, max.m_x), min.m_x ),
							Math.max( Math.min(m_y, max.m_y), min.m_y )
						);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function ClampInto( min:Vector2, max:Vector2 ):Vector2
		{
			m_x = Math.max( Math.min( m_x, max.m_x ), min.m_x );
			m_y = Math.max( Math.min( m_y, max.m_y ), min.m_y );
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Perp():Vector2
		{
			return new Vector2( -m_y, m_x);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Neg():Vector2
		{
			return new Vector2( -m_x, -m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function NegTo( ):Vector2
		{
			m_x = -m_x;
			m_y = -m_y;
			
			return this;
		}
	
		/// <summary>
		/// 
		/// </summary>	
		public function Equal(v:Vector2):Boolean
		{
			return m_x == v.m_x && m_y == v.m_y;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public static function FromAngle(angle:Number) : Vector2
		{
			return new Vector2(Math.cos(angle), Math.sin(angle));
		}

		/// <summary>
		/// 
		/// </summary>	
		public function ToAngle() : Number
		{
			var angle:Number = Math.atan2(m_y, m_x);

			// make the returned range 0 -> 2*PI
			if (angle < 0.0)
			{
				angle += Constants.kTwoPi;
			}
			return angle;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public static function RandomRadius(r:Number):Vector2
		{
			return new 	Vector2
						(
							Math.random() * 2 - 1,
							Math.random() * 2 - 1
						).MulScalar( r );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public static function FromPoint( point:Point ):Vector2
		{
			return new Vector2(point.x,point.y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Point():Point
		{
			return new Point(m_x, m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Clear( ):void
		{
			m_x=0;
			m_y=0;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Clone( ):Vector2
		{
			return new Vector2(m_x, m_y);
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function toString( ):String
		{
			return "x="+m_x+",y="+m_y;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function CloneInto( v:Vector2 ):Vector2
		{
			m_x = v.m_x;
			m_y = v.m_y;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function MaxInto( b:Vector2 ):Vector2
		{
			m_x = Math.max( m_x, b.m_x );
			m_y = Math.max( m_y, b.m_y );
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function MinInto( b:Vector2 ):Vector2
		{
			m_x = Math.min( m_x, b.m_x );
			m_y = Math.min( m_y, b.m_y );
			
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Min( b:Vector2 ):Vector2
		{
			return new Vector2( m_x, m_y ).MinInto( b );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Max( b:Vector2 ):Vector2
		{
			return new Vector2( m_x, m_y ).MaxInto( b );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function AbsTo():void
		{
			m_x = Math.abs( m_x );
			m_y = Math.abs( m_y );
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function UnitTo() : Vector2
		{
			var invLen : Number = 1.0 / m_Len;
			m_x *= invLen;
			m_y *= invLen;
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function IsNaN( ):Boolean
		{
			return isNaN(m_x)||isNaN(m_y);
		}
		
		/// <summary>
		/// Get the largest coordinate and return a signed, unit vector containing only that coordinate
		/// </summary>	
		public function get m_MajorAxis( ):Vector2
		{
			if ( Math.abs( m_x )>Math.abs( m_y ) )
			{
				return new Vector2( Scalar.Sign(m_x), 0 );
			}
			else 
			{
				return new Vector2( 0, Scalar.Sign(m_y) );
			}
		}
		
		/// <summary>
		/// 
		/// </summary>
		public function FloorTo( ):Vector2
		{
			m_x = Math.floor( m_x );
			m_y = Math.floor( m_y );
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>
		public function RoundTo( ):Vector2
		{
			m_x = Math.floor( m_x+0.5 );
			m_y = Math.floor( m_y+0.5 );
			return this;
		}
		
		/// <summary>
		/// 
		/// </summary>
		public function Wedge( v:Vector2 ):Number
		{
			return m_x*v.m_y-m_y*v.m_x;
		}
	}
}