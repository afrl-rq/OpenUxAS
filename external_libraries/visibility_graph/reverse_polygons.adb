with vertex_list;

package body reverse_polygons with SPARK_Mode is
   
   use vertex_list;

   -- Function to reverse the vertices of a polygon
   function reverse_polygon (G : polygon_2d) return polygon_2d is
      vertices_reversed : bounded_vertex_array := (others => (x => 0.0, y => 0.0));
   begin
      for i in 0 .. G.num_vertices - 1 loop
         vertices_reversed(i) := G.vertices(rev_polygon_index(G, i));
      end loop;
      
      return (num_vertices => G.num_vertices, vertices => vertices_reversed);
   end reverse_polygon;

end reverse_polygons;
