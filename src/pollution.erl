%%%-------------------------------------------------------------------
%%% @author Kuba
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. kwi 2017 22:19
%%%-------------------------------------------------------------------
-module(pollution).
-author("Kuba").

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getHourlyMean/4]).

-record(monitor, {stations = []}).
-record(station, {name, coordinates, values = []}).
-record(value, {time, type, value}).

createMonitor() ->
  #monitor{}.

addStation(Name, {Lat, Long}, Monitor) ->
  Stations = Monitor#monitor.stations,
  case (findStation(Name, {Lat, Long}, Stations)) of
    true -> {error, "Such station already exists!"};
    false ->
      Station = #station{name = Name, coordinates = {Lat,Long}},
      Monitor#monitor{stations = [Station|Stations]}
  end.

findStation(_, _, []) -> false;
findStation(Name, Coords, [#station{name=Name, coordinates=_, values=_} | _]) -> true;
findStation(Name, Coords, [#station{name=_, coordinates=Coords, values=_} | _]) -> true;
findStation(Name, Coords, [_ | T]) -> findStation(Name, Coords, T).

findValue(_, _, _, []) -> false;
findValue(Station, Time, Type, [#station{name=Station, coordinates=_, values=[]} | _]) -> false;
findValue(Station, Time, Type, [#station{name=_, coordinates=Station, values=[]} | _]) -> false;
findValue(Station, Time, Type, [#station{name=_, coordinates=Station, values=[#value{time=Time, type=Type, value=_} | _]} | _]) -> true;
findValue(Station, Time, Type, [#station{name=Station, coordinates=_, values=[#value{time=Time, type=Type, value=_} | _]} | _]) -> true;
findValue(Station, Time, Type, [_ | T]) -> findValue(Station, Time, Type, T).

reallyAddValue(Station, V, [#station{name=Station, coordinates=C, values=T} | Tail], H) ->
  H++[#station{name=Station, coordinates = C, values = [V | T]} | Tail];
reallyAddValue(Station, V, [#station{name=N, coordinates=Station, values=T} | Tail], H) ->
  H++[#station{name=N, coordinates = Station, values = [V | T]} | Tail];
reallyAddValue(Station, V, [H | T], OldH) -> reallyAddValue(Station, V, T, OldH++[H]).

addValue(Station, Time, Type, Value, Monitor) ->
  Stations = Monitor#monitor.stations,
  case findStation(Station, Station, Stations) of
    false -> {error, "Such station does not exist!"};
    true ->
      case findValue(Station, Time, Type, Stations) of
        false ->
          V = #value{time = Time, type = Type, value = Value},
          UpdatedStations = reallyAddValue(Station, V, Stations, []),
          #monitor{stations = UpdatedStations};
        true -> {error, "Value of such type/data already exists!"}
      end
  end.

reallyRemoveValue(_, _, _, []) -> false;
reallyRemoveValue(Station, Time, Type, [#station{name=Station, coordinates=_, values=[]} | _]) -> false;
reallyRemoveValue(Station, Time, Type, [#station{name=_, coordinates=Station, values=[]} | _]) -> false;
reallyRemoveValue(Station, Time, Type, [#station{name=N, coordinates=Station, values=[#value{time=Time, type=Type, value=_} | VT]} | ST]) ->
  [#station{name=N, coordinates = Station, values = VT} | ST];
reallyRemoveValue(Station, Time, Type, [#station{name=Station, coordinates=C, values=[#value{time=Time, type=Type, value=_} | VT]} | ST]) ->
  [#station{name=Station, coordinates = C, values = VT} | ST];
reallyRemoveValue(Station, Time, Type, [_ | T]) -> reallyRemoveValue(Station, Time, Type, T).

removeValue(Station, Time, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  UpdatedStations = reallyRemoveValue(Station, Time, Type, Stations),
  case UpdatedStations of
    false -> {error, "No such value!"};
    _ -> Monitor#monitor{stations = UpdatedStations}
  end.

reallyGetOneValue(_, _, _, []) -> false;
reallyGetOneValue(Station, Time, Type, [#station{name=Station, coordinates=_, values=[]} | _]) -> false;
reallyGetOneValue(Station, Time, Type, [#station{name=_, coordinates=Station, values=[]} | _]) -> false;
reallyGetOneValue(Station, Time, Type, [#station{name=_, coordinates=Station, values=[#value{time=Time, type=Type, value=V} | _]} | _]) ->
  V;
reallyGetOneValue(Station, Time, Type, [#station{name=Station, coordinates=_, values=[#value{time=Time, type=Type, value=V} | _]} | _]) ->
  V;
reallyGetOneValue(Station, Time, Type, [_ | T]) -> reallyRemoveValue(Station, Time, Type, T).

getOneValue(Station, Time, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Value = reallyGetOneValue(Station, Time, Type, Stations),
  case Value of
    false -> {error, "No such value or station!"};
    _ -> Value
  end.

stationCount(_, _, Count, []) -> Count;
stationCount(Station, Type, Count, [#station{name=Station, coordinates=C, values=[#value{time=_, type=Type, value=_} | VT]} | _]) ->
  stationCount(Station, Type, Count+1, [#station{name=Station, coordinates=C, values=VT}]);
stationCount(Station, Type, Count, [#station{name=N, coordinates=Station, values=[#value{time=_, type=Type, value=_} | VT]} | _]) ->
  stationCount(Station, Type, Count+1, [#station{name=N, coordinates=Station, values=VT}]);
stationCount(Station, Type, Count, [#station{name=_, coordinates=Station, values=[]} | _]) ->
  Count;
stationCount(Station, Type, Count, [#station{name=Station, coordinates=_, values=[]} | _]) ->
  Count;
stationCount(Station, Type, Count, [_ | T]) -> stationCount(Station, Type, Count, T).

stationSum(_, _, Sum, []) -> Sum;
stationSum(Station, Type, Sum, [#station{name=Station, coordinates=C, values=[#value{time=_, type=Type, value=V} | VT]} | _]) ->
  stationSum(Station, Type, Sum+V, [#station{name=Station, coordinates=C, values=VT}]);
stationSum(Station, Type, Sum, [#station{name=N, coordinates=Station, values=[#value{time=_, type=Type, value=V} | VT]} | _]) ->
  stationSum(Station, Type, Sum+V, [#station{name=N, coordinates=Station, values=VT}]);
stationSum(Station, Type, Sum, [#station{name=_, coordinates=Station, values=[]} | _]) ->
  Sum;
stationSum(Station, Type, Sum, [#station{name=Station, coordinates=_, values=[]} | _]) ->
  Sum;
stationSum(Station, Type, Sum, [_ | T]) -> stationSum(Station, Type, Sum, T).

getStationMean(Station, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Count = stationCount(Station, Type, 0, Stations),
  case Count of
    0 -> {error, "No matches found (or wrong station)!"};
    _ ->
      Sum = stationSum(Station, Type, 0, Stations),
      Mean = Sum/Count,
      Mean
  end.

dailyStationCount(_, _, Count, []) -> Count;
dailyStationCount({Year, Month, Day}, Type, Count, [#station{name=N, coordinates=C, values=[#value{time={{Year, Month, Day},{_,_,_}}, type=Type, value=_} | VT]} | ST]) ->
  dailyStationCount({Year, Month, Day}, Type, Count+1, [#station{name=N, coordinates=C, values=VT} | ST]);
dailyStationCount({Year, Month, Day}, Type, Count, [_ | T]) -> dailyStationCount({Year, Month, Day}, Type, Count, T).

dailyStationSum(_, _, Sum, []) -> Sum;
dailyStationSum({Year, Month, Day}, Type, Sum, [#station{name=N, coordinates=C, values=[#value{time={{Year, Month, Day},{_,_,_}}, type=Type, value=V} | VT]} | ST]) ->
  dailyStationSum({Year, Month, Day}, Type, Sum+V, [#station{name=N, coordinates=C, values=VT} | ST]);
dailyStationSum({Year, Month, Day}, Type, Sum, [_ | T]) -> dailyStationSum({Year, Month, Day}, Type, Sum, T).

getDailyMean({Year, Month, Day}, Type, Monitor) ->
  Stations = Monitor#monitor.stations,
  Count = dailyStationCount({Year, Month, Day}, Type, 0, Stations),
  case Count of
    0 -> {error, "No matches found!"};
    _ ->
      Sum = dailyStationSum({Year, Month, Day}, Type, 0, Stations),
      Mean = Sum/Count,
      Mean
  end.

getHourlyMeanCount(_, _, _, Count, []) -> Count;
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=Station, coordinates=C, values=[#value{time={{_,_,_},{Hour,_,_}}, type=Type, value=_} | VT]} | _]) ->
  getHourlyMeanCount(Station, Type, Hour, Count+1, [#station{name=Station, coordinates=C, values=VT}]);
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=N, coordinates=Station, values=[#value{time={{_,_,_},{Hour,_,_}}, type=Type, value=_} | VT]} | _]) ->
  getHourlyMeanCount(Station, Type, Hour, Count+1, [#station{name=N, coordinates=Station, values=VT}]);
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=_, coordinates=Station, values=[]} | _]) ->
  Count;
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=Station, coordinates=_, values=[]} | _]) ->
  Count;
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=Station, coordinates=C, values=[_ | VT]} | _]) ->%edit
  getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=Station, coordinates=C, values=VT}]);
getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=N, coordinates=Station, values=[_ | VT]} | _]) ->%edit
  getHourlyMeanCount(Station, Type, Hour, Count, [#station{name=N, coordinates=Station, values=VT}]);
getHourlyMeanCount(Station, Type, Hour, Count, [_ | T]) -> getHourlyMeanCount(Station, Type, Hour, Count, T).

getHourlyMeanSum(_, _, _, Sum, []) -> Sum;
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=Station, coordinates=C, values=[#value{time={{_,_,_},{Hour,_,_}}, type=Type, value=V} | VT]} | _]) ->
  getHourlyMeanSum(Station, Type, Hour, Sum+V, [#station{name=Station, coordinates=C, values=VT}]);
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=N, coordinates=Station, values=[#value{time={{_,_,_},{Hour,_,_}}, type=Type, value=V} | VT]} | _]) ->
  getHourlyMeanSum(Station, Type, Hour, Sum+V, [#station{name=N, coordinates=Station, values=VT}]);
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=_, coordinates=Station, values=[]} | _]) ->
  Sum;
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=Station, coordinates=_, values=[]} | _]) ->
  Sum;
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=Station, coordinates=C, values=[_ | VT]} | _]) ->%edit
  getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=Station, coordinates=C, values=VT}]);
getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=N, coordinates=Station, values=[_ | VT]} | _]) ->%edit
  getHourlyMeanSum(Station, Type, Hour, Sum, [#station{name=N, coordinates=Station, values=VT}]);
getHourlyMeanSum(Station, Type, Hour, Sum, [_ | T]) -> getHourlyMeanSum(Station, Type, Hour, Sum, T).

getHourlyMean(Station, Type, Hour, Monitor) ->
  Stations = Monitor#monitor.stations,
  Count = getHourlyMeanCount(Station, Type, Hour, 0, Stations),
  case Count of
    0 -> {error, "No data matched!"};
    _ ->
      Sum = getHourlyMeanSum(Station, Type, Hour, 0, Stations),
      Mean = Sum/Count,
      Mean
  end.