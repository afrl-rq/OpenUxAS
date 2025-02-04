-- reverse_polygons.ads
-- SPARK Ada specification for PVS functions: rev_polygon_index and reverse_polygon
-- Dependencies: polygons_2d

with polygons_2d;

package reverse_polygons with SPARK_Mode is

   use polygons_2d;

   -- Function to compute the reverse index in a polygon
   -- PVS:
   -- rev_polygon_index(G: polygon_2d, i: below(G`num_vertices)): below(G`num_vertices) =
   --   IF i = 0 THEN 0 ELSE G`num_vertices - i ENDIF
   function rev_polygon_index(G : polygon_2d; i : Natural) return Natural is
     (if i = 0 then 0 else G.num_vertices - i)
     with
       Pre => i < G.num_vertices,
       Post => rev_polygon_index'Result < G.num_vertices;

   -- Function to reverse the vertices of a polygon
   -- PVS:
   -- reverse_polygon(G: polygon_2d): polygon_2d =
   --   (# num_vertices := G`num_vertices,
   --      vertices := LAMBDA (i: below(G`num_vertices)):
   --                    G`vertices(rev_polygon_index(G, i))
   --    #)
   function reverse_polygon(G : polygon_2d) return polygon_2d;

end reverse_polygons;
