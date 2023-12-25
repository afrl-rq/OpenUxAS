with Ada.Numerics.Generic_Elementary_Functions;

package body Unit_Conversion_Utilities is

   package Real_Elementary_Fuctions is new Ada.Numerics.Generic_Elementary_Functions (Real);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This              : out Unit_Converter;
      LatitudeInit_Rad  : Real;
      LongitudeInit_Rad : Real)
   is
      dDenominatorMeridional : Real;
      dDenominatorTransverse : Real;

      use Real_Elementary_Fuctions;

      function Pow (Base, Exp : Real) return Real renames Real_Elementary_Fuctions."**";
   begin
         --  //assumes that the conversions will all take place within the local area of the initial latitude/longitude.
         --  m_dLatitudeInitial_rad = dLatitudeInit_rad;
         This.m_dLatitudeInitial_rad := LatitudeInit_Rad;
         --  m_dLongitudeInitial_rad = dLongitudeInit_rad;
         This.m_dLongitudeInitial_rad := LongitudeInit_Rad;

         --  double dDenominatorMeridional = std::pow((1.0 - (m_dEccentricitySquared * std::pow(std::sin(dLatitudeInit_rad), 2.0))), (3.0 / 2.0));
         dDenominatorMeridional := Pow ((1.0 - (dEccentricitySquared * Pow (Sin (LatitudeInit_Rad), 2.0))), (3.0 / 2.0));
         --  assert(dDenominatorMeridional > 0.0);
         pragma Assert (dDenominatorMeridional > 0.0);
         --  m_dRadiusMeridional_m = (dDenominatorMeridional <= 0.0) ? (0.0) : (m_dRadiusEquatorial_m * (1.0 - m_dEccentricitySquared) / dDenominatorMeridional);
         This.m_dRadiusMeridional_m := (if dDenominatorMeridional <= 0.0 then 0.0 else (dRadiusEquatorial_m * (1.0 - dEccentricitySquared) / dDenominatorMeridional));
         --  double dDenominatorTransverse = pow((1.0 - (m_dEccentricitySquared * std::pow(std::sin(dLatitudeInit_rad), 2.0))), 0.5);
         dDenominatorTransverse := Pow ((1.0 - (dEccentricitySquared * Pow (Sin (LatitudeInit_Rad), 2.0))), 0.5);
         --  assert(dDenominatorTransverse > 0.0);
         pragma Assert (dDenominatorTransverse > 0.0);
         --  m_dRadiusTransverse_m = (dDenominatorTransverse <= 0.0) ? (0.0) : (m_dRadiusEquatorial_m / dDenominatorTransverse);
         This.m_dRadiusTransverse_m := (if dDenominatorTransverse <= 0.0 then 0.0 else (dRadiusEquatorial_m / dDenominatorTransverse));
         --  m_dRadiusSmallCircleLatitude_m = m_dRadiusTransverse_m * cos(dLatitudeInit_rad);
         This.m_dRadiusSmallCircleLatitude_m := This.m_dRadiusTransverse_m * Cos (LatitudeInit_Rad);
   end Initialize;

   -------------------------------------------------
   -- Convert_LatLong_Degrees_To_NorthEast_Meters --
   -------------------------------------------------

   procedure Convert_LatLong_Degrees_To_NorthEast_Meters
     (This          : in Unit_Converter;
      Latitude_Deg  : Real;
      Longitude_Deg : Real;
      North         : out Real;
      East          : out Real)
   is
      DegreesToRadians : constant := 180.0 / Ada.Numerics.Pi;

      --  double dLatitude_rad = dLatitude_deg * n_Const::c_Convert::dDegreesToRadians();
      Latitude_Rad : constant Real := Latitude_Deg * DegreesToRadians;
      --  double dLongitude_rad = dLongitude_deg * n_Const::c_Convert::dDegreesToRadians();
      Longitude_Rad  : constant Real := Longitude_Deg * DegreesToRadians;
   begin
      --  //assumes that the conversions will all take place within the local area of the init longitude.
      --  if (!m_bInitialized)
      --  {
      --      Initialize(dLatitude_rad, dLongitude_rad);
      --  }
      --  dNorth_m = m_dRadiusMeridional_m * (dLatitude_rad - m_dLatitudeInitial_rad);
      North := This.m_dRadiusMeridional_m * (Latitude_Rad - This.m_dLatitudeInitial_rad);
      --  dEast_m = m_dRadiusSmallCircleLatitude_m * (dLongitude_rad - m_dLongitudeInitial_rad);
      East := This.m_dRadiusSmallCircleLatitude_m * (Longitude_Rad - This.m_dLongitudeInitial_rad);
   end Convert_LatLong_Degrees_To_NorthEast_Meters;

   -------------------------------------------------
   -- Convert_NorthEast_Meters_To_LatLong_Degrees --
   -------------------------------------------------

   procedure Convert_NorthEast_Meters_To_LatLong_Degrees
     (This          : in Unit_Converter;
      North         : Real;
      East          : Real;
      Latitude_Deg  : out Real;
      Longitude_Deg : out Real)
   is
      RadiansToDegrees : constant := Ada.Numerics.Pi / 180.0;

   begin

      if This.m_dRadiusMeridional_m <= 0.0 then
         Latitude_Deg := 0.0;
      else
         Latitude_Deg := North / This.m_dRadiusMeridional_m + This.m_dLatitudeInitial_rad;
      end if;

      if This.m_dRadiusSmallCircleLatitude_m <= 0.0 then
         Longitude_Deg := 0.0;
      else
         Longitude_Deg := East / This.m_dRadiusSmallCircleLatitude_m + This.m_dLongitudeInitial_rad;
      end if;

   end Convert_NorthEast_Meters_To_LatLong_Degrees;

   u_Converter : Unit_Converter := (0.0, 0.0, 0.0, 0.0, 0.0);
end Unit_Conversion_Utilities;
