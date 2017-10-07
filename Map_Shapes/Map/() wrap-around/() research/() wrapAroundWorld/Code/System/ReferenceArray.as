package Code.System
{
	import Code.System.*;
	
	/// <summary>
	/// Simple array class for efficiently storing references to objects - warning the iteration order will not be consistent after calling Remove()
	/// </summary>	
	public class ReferenceArray
	{
		private var m_counter:int;
		private var m_maxObjects:int;
		private var m_data:Array;
		private var m_type:Class;
		
		/// <summary>
		/// Default is infinte object count
		/// </summary>	
		public function ReferenceArray( type:Class, maxObjects:int=0 )
		{
			if ( maxObjects!=0 )
			{
				m_data = new Array( maxObjects );
				m_maxObjects=maxObjects;
			}
			else 
			{
				m_data = new Array( );
				m_maxObjects = int.MAX_VALUE;
			}
			
			m_counter=0;
			m_type=type;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Add( obj:* ):void
		{
			Assert( m_counter<m_maxObjects, "ReferenceArray.Add(): pool out of space!" );
			Assert( obj is m_type, "ReferenceArray.Add(): wrong object type!");
			
			m_data[m_counter++]=obj;
		}
		
		/// <summary>
		/// Returns -1 on failure
		/// </summary>	
		public function FindIndex( obj:* ):int
		{
			for ( var i:int = 0; i<m_counter; i++ )
			{
				if ( m_data[i]==obj )
				{
					return i;
				}
			}
			
			return -1;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Contains( obj:* ):Boolean
		{
			return FindIndex( obj )!=-1;
		}
		
		/// <summary>
		/// Remove an item from the array: be aware - if iterating over the array when calling this function
		/// you will need to re-do the current iteration if this function returns true, because a new 
		/// entry will have been moved into the current slot:
		///
		/// for (var i:int=0; i<tempArray.m_NumObjects; i++)
		/// {
		///		if (tempArray.Remove(obj))
		///		{
		///			i--;
		///		}
		///	}
		/// </summary>
		public function Remove( obj:* ):Boolean
		{
			Assert( obj is m_type, "ReferenceArray.Remove(): wrong type for collection!" );
			var i:int = FindIndex(obj);
			
			if (i!=-1)
			{
				return RemoveIndex( i );
			}
				
			return false;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function RemoveIndex( i:int ):Boolean
		{
			Assert( i>=0&&i<m_Num, "ReferenceArray.RemoveIndex(): index out of bounds!" );
			var reeval:Boolean = false;
			
			m_counter--;
				
			if ( i!=m_counter )
			{
				// move last obj into spare slot
				m_data[i] = m_data[m_counter];
				
				// caller will need to re-evaluate if iterating over objects
				reeval = true;
			}
			
			// clear our reference to this object
			m_data[m_counter] = null;
			
			return reeval;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function get m_Num():int
		{
			return m_counter;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Get(i:int):*
		{
			Assert(i<m_counter, "ReferenceArray.GetObject(): index out of range!");
			return m_data[i];
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Clear():void
		{
			for ( var i:int = 0; i<m_counter; i++ )
			{
				m_data[i] = null;
			}
			m_counter=0;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function ForEach( action:Function ):void
		{
			for ( var i:int = 0; i<m_Num; i++ )
			{
				action( Get( i ) );
			}
		}
	}
}
