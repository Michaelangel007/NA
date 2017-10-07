package Code.Geometry
{
	import Code.Maths.Vector2;
	
	public interface IAABB
	{
		function get m_Centre( ):Vector2;
		function get m_HalfExtents( ):Vector2;
	}
}
