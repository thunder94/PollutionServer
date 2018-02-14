%%%-------------------------------------------------------------------
%%% @author Kuba
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. maj 2017 15:25
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Kuba").

%% API
-export([start/0, stop/0, init/0, loop/1, addStation/2, addValue/4]).
-export([removeValue/3, getOneValue/3, getDailyMean/2, getStationMean/2, getHourlyMean/3]).

start() ->
  register(pollution_serv, spawn(?MODULE, init, [])).

init() ->
  M = pollution:createMonitor(),
  loop(M).

loop(Monitor) ->
  receive
    stop -> ok;
    {Pid, addStation, Name, Lat, Long} -> UpdatedMonitor = pollution:addStation(Name, {Lat, Long}, Monitor),
      case UpdatedMonitor of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;
    {Pid, addValue, Station, Time, Type, Value} -> UpdatedMonitor = pollution:addValue(Station, Time, Type, Value, Monitor),
      case UpdatedMonitor of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;
    {Pid, removeValue, Station, Time, Type} -> UpdatedMonitor = pollution:removeValue(Station, Time, Type, Monitor),
      case UpdatedMonitor of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! ok,
          loop(UpdatedMonitor)
      end;
    {Pid, getOneValue, Station, Time, Type} -> Value = pollution:getOneValue(Station, Time, Type, Monitor),
      case Value of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! Value,
          loop(Monitor)
      end;
    {Pid, getStationMean, Station, Type} -> Value = pollution:getStationMean(Station, Type, Monitor),
      case Value of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! Value,
          loop(Monitor)
      end;
    {Pid, getDailyMean, Year, Month, Day, Type} -> Value = pollution:getDailyMean({Year, Month, Day}, Type, Monitor),
      case Value of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! Value,
          loop(Monitor)
      end;
    {Pid, getHourlyMean, Station, Type, Hour} -> Value = pollution:getHourlyMean(Station, Type, Hour, Monitor),
      case Value of
        {error, e} ->
          Pid ! e,
          loop(Monitor);
        _ ->
          Pid ! Value,
          loop(Monitor)
      end;
    _ -> {error, "Unexpected operation!"}
  end.

addStation(Name, {Lat, Long}) ->
  pollution_serv ! {self(), addStation, Name, Lat, Long},
  receive
    M -> io:format("Result: ~p~n", [M])
  end.

addValue(Station, Time, Type, Value) ->
  pollution_serv ! {self(), addValue, Station, Time, Type, Value}.

removeValue(Station, Time, Type) ->
  pollution_serv ! {self(), removeValue, Station, Time, Type}.

getOneValue(Station, Time, Type) ->
  pollution_serv ! {self(), getOneValue, Station, Time, Type}.

getStationMean(Station, Type) ->
  pollution_serv ! {self(), getStationMean, Station, Type}.

getDailyMean({Year, Month, Day}, Type) ->
  pollution_serv ! {self(), getDailyMean, Year, Month, Day, Type}.

getHourlyMean(Station, Type, Hour) ->
  pollution_serv ! {self(), getHourlyMean, Station, Type, Hour}.

stop() ->
  pollution_serv ! stop.