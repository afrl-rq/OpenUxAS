--  with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with definitions; use definitions;
with Ada.Numerics.Generic_Elementary_Functions;

package FlatEarth is

   package SafeReal64_Elementary_Functions is 
     new Ada.Numerics.Generic_Elementary_Functions (SafeReal64);
   use SafeReal64_Elementary_Functions;
      
   type FlatEarthObject is tagged private;
   
   procedure Initialize (feo :  out FlatEarthObject;
                        latitude_initial_rad : SafeReal64;
                        longitude_initial_rad : SafeReal64);
   
   --Convert Latitude Longitude to North East
   procedure ConvertLatitudeLongitude_rad_ToNorthEast_ft
     (feo : in out FlatEarthObject;
      latitude_rad : SafeReal64;
      longitude_rad : SafeReal64;
      north_ft : out SafeReal64;
      east_ft : out SafeReal64);

   procedure ConvertLatitudeLongitude_rad_ToNorthEast_m
     (feo : in out FlatEarthObject;
      latitude_rad : SafeReal64;
      longitude_rad : SafeReal64;
      north_m : out SafeReal64;
      east_m : out SafeReal64);
   
   procedure ConvertLatitudeLongitude_deg_ToNorthEast_m
     (feo : in out FlatEarthObject;
      latitude_deg : SafeReal64;
      longitude_deg : SafeReal64;
      north_m : out SafeReal64;
      east_m : out SafeReal64);
   
   procedure ConvertLatitudeLongitude_deg_ToNorthEast_ft
     (feo : in out FlatEarthObject;
      latitude_deg : SafeReal64;
      longitude_deg : SafeReal64;
      north_ft : out SafeReal64;
      east_ft : out SafeReal64);
   
   --Convert from North East to Latitude Longitude
   procedure ConvertNorthEast_m_ToLatitudeLongitude_rad 
     (feo : FlatEarthObject;
      north_m : SafeReal64;
      east_m : SafeReal64;
      latitude_rad : out SafeReal64;
      longitude_rad : out SafeReal64);
   
   procedure ConvertNorthEast_m_ToLatitudeLongitude_deg 
     (feo : FlatEarthObject;
      north_m : SafeReal64;
      east_m : SafeReal64;
      latitude_deg : out SafeReal64;
      longitude_deg : out SafeReal64);
   
   procedure ConvertNorthEast_ft_ToLatitudeLongitude_rad 
     (feo : FlatEarthObject;
      north_ft : SafeReal64;
      east_ft : SafeReal64;
      latitude_rad : out SafeReal64;
      longitude_rad : out SafeReal64);
   
   procedure ConvertNorthEast_ft_ToLatitudeLongitude_deg 
     (feo : FlatEarthObject;
      north_ft : SafeReal64;
      east_ft : SafeReal64;
      latitude_deg : out SafeReal64;
      longitude_deg : out SafeReal64);
   
   --Linear distances
   function GetLinearDistance_m_Lat1Long1_deg_To_Lat2Long2_deg 
     (feo : in out FlatEarthObject;
      latitude1_deg : SafeReal64;
      longitude1_deg : SafeReal64;
      latitude2_deg : SafeReal64;
      longitude2_deg : SafeReal64) return SafeReal64;
   
   function GetLinearDistance_m_Lat1Long1_rad_To_Lat2Long2_rad 
     (feo : in out FlatEarthObject;
      latitude1_rad : SafeReal64;
      longitude1_rad : SafeReal64;
      latitude2_rad : SafeReal64;
      longitude2_rad : SafeReal64) return SafeReal64;
   
   --Constants
   RadiusEquatorial_m : constant SafeReal64 := 6_378_135.0;
   Flattening : constant SafeReal64 := 3.352810664724998e-003;
   EccentricitySquared : constant SafeReal64 := 6.694379990096503e-003;
   convertMetersToFeet : constant SafeReal64 := 3.280839895;
   convertFeetToMeters : constant SafeReal64 := 0.3048;
   convertDegreesToRadians : constant SafeReal64 := 0.01745329251994;
   convertRadiansToDegrees : constant SafeReal64 := 57.29577951308232;
   
private
   type FlatEarthObject is tagged record
      LatitudeInitial_rad : SafeReal64 := 0.0;
      LongitudeInitial_rad : SafeReal64 := 0.0;
      RadiusMeridional_m : SafeReal64 := 0.0;
      RadiusTransverse_m : SafeReal64 := 0.0;
      RadiusSmallCircleLatitude_m : SafeReal64 := 0.0;
      isInitialized : Boolean := False;
   end record;
   
end FlatEarth;
