package Code.System
{
	import Code.System.*;
	
	public class Pool
	{
		private var m_counter:int;
		private var m_maxObjects:int;
		private var m_pool:Array;
		private var m_type:Class;
		private var m_deallocateCalled:Boolean;
		
		/// <summary>
		/// 
		/// </summary>	
		public function Pool( type:Class, maxObjects:int )
		{
			m_pool = new Array( maxObjects );
			
			// construct all objects
			for (var i:int=0; i<maxObjects; i++)
			{
				m_pool[i] = new type();
			}
			
			m_counter=0;
			m_type=type;
			m_maxObjects=maxObjects;
			m_deallocateCalled = false;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Allocate( ...args ):*
		{
			//Assert( m_counter<m_maxObjects, "Pool.GetObject(): pool out of space for type " + m_type );
			var obj:*;
			
			if ( m_counter<m_maxObjects )
			{
				obj = m_pool[m_counter++];
			}
			else 
			{
				obj = new m_type( );
			}
			
			if ( args.length==0 )
			{
				obj.Initialise( );
			}
			else if ( args.length==1 )
			{
				obj.Initialise( args[0] );
			}
			else if ( args.length==2 )
			{
				obj.Initialise( args[0], args[1] );
			}
			else if ( args.length==3 )
			{
				obj.Initialise( args[0], args[1], args[2] );
			}
			else if ( args.length==4 )
			{
				obj.Initialise( args[0], args[1], args[2], args[3] );
			}
			else if ( args.length==5 )
			{
				obj.Initialise( args[0], args[1], args[2], args[3], args[4] );
			}
			else if ( args.length==6 )
			{
				obj.Initialise( args[0], args[1], args[2], args[3], args[4], args[5] );
			}
			else if ( args.length==7 )
			{
				obj.Initialise( args[0], args[1], args[2], args[3], args[4], args[5], args[6] );
			}
			else 
			{
				throw new UnexpectedCaseException;
			}
			
			return obj;
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Deallocate( obj:* ):void
		{
			Assert( typeof( obj )==typeof( m_type ), "Pool.Deallocate(): object doesn't belong to this pool!" );
			Assert( m_counter>0, "Pool.Deallocate(): too many deallocations!");
			m_pool[--m_counter] = obj;
			m_deallocateCalled = true;
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
			Assert(i<m_counter, "Pool.Get(): index out of range!");
			return m_pool[i];
		}
		
		/// <summary>
		/// 
		/// </summary>	
		public function Clear():void
		{
			Assert( !m_deallocateCalled, "Pool.Clear(): Deallocate() already called! Clear cannot be used with this!" );
			m_counter=0;
		}
	}
}
