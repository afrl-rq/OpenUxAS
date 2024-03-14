with polygon_lemmas;

package body polygon_attributes with SPARK_mode is

  use polygon_lemmas;

  --   % To determine whether a point lies between the rays induced by two
  --   % edges, we differentiate the inward and outward cases.  The inward
  --   % case uses the same ccw logic after first swapping the edges and
  --   % their endpoints.
  --   point_between_edges?(outward: bool, G: simple_polygon_2d,
  --                        i: below(G`num_vertices))(p: point_2d): bool =
  --     LET s = edges_of_polygon(G)(prev_index(G, i)),
  --         e = edges_of_polygon(G)(i)
  --     IN IF outward THEN between_rays?(s, e)(p)
  --                   ELSE between_rays?(reverse_segment(e), reverse_segment(s))(p)
  --        ENDIF
  function point_between_edges(outward: Boolean; G: simple_polygon_2d;
                               i: Natural; p: point_2d) return Boolean is
    retval: Boolean;
  begin
    Lemma_Pts_Can_Subtract_Implies_Segment_Endpts_Can_Subtract(G, p);
    if outward then
      retval := is_between_rays(edges_of_polygon(G,  prev_index(G, i)),
                                edges_of_polygon(G, i), p);
    else
      retval := is_between_rays(reverse_segment(edges_of_polygon(G, i)),
                                reverse_segment(edges_of_polygon(G, prev_index(G, i))), p);
    end if;
    return retval;
  end point_between_edges;

end polygon_attributes;
