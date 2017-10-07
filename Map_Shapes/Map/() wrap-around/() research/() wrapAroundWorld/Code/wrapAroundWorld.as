package Code
{
	import flash.display.*;
	import flash.events.*;
	
	import Code.System.*;
	import Code.Maths.*;
	import Code.Geometry.*;
	import Code.Graphics.*;
	
	[SWF(width="640", height="360", backgroundColor="#EAEAEA")]
	public class wrapAroundWorld extends GameLoop
	{
		private const kAColour:uint = 0xff0000;
		private const kBColour:uint = 0x00ff00;
		private const kCColour:uint = 0x0000ff;
		
		private var m_screenAabb:AABB;
		private var m_box:Box;
		private var m_overlapBoxs:Vector.<Box>;
		private var m_dragObj:Box;
		private var m_dragOffset:Vector2;
		
		/**
		 */
		public function wrapAroundWorld( )
		{
			super( );
			
			m_screenAabb = new AABB( Constants.kScreenDimensions.MulScalar( 0.5 ), Constants.kScreenDimensions.MulScalar( 0.5 ) );
			m_box = new Box( new AABB( Constants.kScreenDimensions.MulScalar(0.5), new Vector2( 100, 50 ) ) );
			
			m_overlapBoxs = new Vector.<Box>( );
			
			this.addChild( m_box );
			
			this.stage.addEventListener( MouseEvent.MOUSE_DOWN, OnMouseDown );
			this.stage.addEventListener( MouseEvent.MOUSE_UP, OnMouseUp );
			this.stage.addEventListener( MouseEvent.MOUSE_MOVE, OnMouseMove );
			
			m_dragObj = null;
		}
		
		/**
		 */
		private function OnMouseDown( e:MouseEvent ):void
		{
			var clickPos:Vector2 = new Vector2( e.stageX, e.stageY );
			if ( m_box.Within( clickPos ) )
			{
				m_dragObj = m_box;
				m_dragOffset = clickPos.Sub( m_box.m_Pos );
			}
		}
		
		/**
		 */
		private function OnMouseUp( e:MouseEvent ):void
		{
			m_dragObj = null;
			this.setChildIndex( m_box, this.numChildren-1 );
		}
		
		/**
		 */
		private function AddOverlapBox( centre:Vector2, halfExtents:Vector2, colour:uint ):void
		{
			var box:Box = new Box( new AABB( centre, halfExtents ), colour );
			m_overlapBoxs.push( box );
			this.addChild( box );
		}
		
		/**
		 */
		private function OnMouseMove( e:MouseEvent ):void
		{
			if ( m_dragObj )
			{
				m_box.m_Pos = new Vector2( e.stageX, e.stageY ).SubFrom( m_dragOffset );
				
				for each(var box:Box in m_overlapBoxs)
				{
					this.removeChild( box );
				}
				m_overlapBoxs = new Vector.<Box>( );
				
				// work out how the box overlaps the world bounds
				var flags:uint = AABB.AabbWithinFlags( m_box.m_Aabb, m_screenAabb );
				
				if ( flags != 0 )
				{
					var overlap:Vector2 = m_box.m_Aabb.m_HalfExtents;
					var overlapCentre:Vector2 = m_box.m_Pos.Clone();
					var centreA:Vector2;
					var centreB:Vector2;
					var centreC:Vector2;
					var combo:Vector2 = null;
					
					
					switch ( flags )
					{
						case OverlapFlags.kMinusX:
						{
							// these cases just produce one extra box
							overlapCentre.m_x += m_screenAabb.m_HalfExtents.m_x*2;
							AddOverlapBox( overlapCentre, overlap, kAColour );
						}
						break;
						
						case OverlapFlags.kMinusY:
						{
							// these cases just produce one extra box
							overlapCentre.m_y += m_screenAabb.m_HalfExtents.m_y*2;
							AddOverlapBox( overlapCentre, overlap, kAColour );
						}
						break;
						
						case OverlapFlags.kPlusX:
						{
							// these cases just produce one extra box
							overlapCentre.m_x -= m_screenAabb.m_HalfExtents.m_x*2;
							AddOverlapBox( overlapCentre, overlap, kAColour );
						}
						break;
						
						case OverlapFlags.kPlusY:
						{
							// these cases just produce one extra box
							overlapCentre.m_y -= m_screenAabb.m_HalfExtents.m_y*2;
							AddOverlapBox( overlapCentre, overlap, kAColour );
						}
						break;
						
						case OverlapFlags.kMinusX|OverlapFlags.kMinusY:
						{
							// these cases just produce three extra boxes!
							combo = m_screenAabb.m_HalfExtents.MulScalar(2);
						}
						break;
						
						case OverlapFlags.kPlusX|OverlapFlags.kMinusY:
						{
							// these cases just produce three extra boxes!
							combo = new Vector2( -m_screenAabb.m_HalfExtents.m_x*2, m_screenAabb.m_HalfExtents.m_y*2 );
						}
						break;
						
						case OverlapFlags.kMinusX|OverlapFlags.kPlusY:
						{
							// these cases just produce three extra boxes!
							combo = new Vector2( m_screenAabb.m_HalfExtents.m_x*2, -m_screenAabb.m_HalfExtents.m_y*2 );
						}
						break;
						
						case OverlapFlags.kPlusX|OverlapFlags.kPlusY:
						{
							// these cases just produce three extra boxes!
							combo = new Vector2( -m_screenAabb.m_HalfExtents.m_x*2, -m_screenAabb.m_HalfExtents.m_y*2 );
						}
						break;
					}
					
					if ( combo!=null )
					{
						// add the three extra boxes
						centreA = overlapCentre.AddX( combo.m_x );
						centreB = overlapCentre.AddY( combo.m_y );
						centreC = overlapCentre.Add( combo );
						
						AddOverlapBox( centreA, overlap, kAColour );
						AddOverlapBox( centreB, overlap, kBColour );
						AddOverlapBox( centreC, overlap, kCColour );
					}
					
					if ( !AABB.Overlap( m_box.m_Aabb, m_screenAabb ) )
					{
						// actually wrap the position of the box
						if ( flags&OverlapFlags.kMinusX )
						{
							m_box.m_Pos.m_x += m_screenAabb.m_HalfExtents.m_x*2;
						}
						if ( flags&OverlapFlags.kMinusY )
						{
							m_box.m_Pos.m_y += m_screenAabb.m_HalfExtents.m_y*2;
						}
						if ( flags&OverlapFlags.kPlusX )
						{
							m_box.m_Pos.m_x -= m_screenAabb.m_HalfExtents.m_x*2;
						}
						if ( flags&OverlapFlags.kPlusY )
						{
							m_box.m_Pos.m_y -= m_screenAabb.m_HalfExtents.m_y*2;
						}
						
						// force the setter to act so it moves the visual
						m_box.m_Pos = m_box.m_Pos;
						
						// stop dragging
						m_dragObj = null;
					}
				}
			}
		}
	}
}
