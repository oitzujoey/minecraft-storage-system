local map map=(function() local t47={} t47[2]=(function() local t48={} t48[0]=201 t48[1]=62 t48[2]=714 return t48 end)() t47[3]=(function() local t49={} t49[0]=179 t49[1]=62 t49[2]=718 return t49 end)() t47[32]=(function() local t50={} t50[0]=180 t50[1]=62 t50[2]=697 return t50 end)() return t47 end)() local home local fuel home=map[2] fuel=map[3] local orientation local desiredorientation orientation=7 local error error=false local ox local oy local oz local x local y local z local dx local dy local dz ox,oy,oz=gps.locate() error=not(turtle.forward()) x,y,z=gps.locate() dx=(x-ox) dz=(z-oz) if not (dx==0) then if (dx<0) then orientation=8 else orientation=9 end end if not (dz==0) then if (dz<0) then orientation=10 else orientation=11 end end function moveToCoordinates(coord) local run run=true while (run and not(error)) do x,y,z=gps.locate() if (y>coord[1]) then error=not(turtle.down()) else if (y<coord[1]) then error=not(turtle.up()) else run=false end end end run=true x,y,z=gps.locate() while (run and not(error)) do if (x==coord[0]) then if (z==coord[2]) then run=false else if (z<coord[2]) then desiredorientation=11 else desiredorientation=10 end end else if (x<coord[0]) then desiredorientation=9 else desiredorientation=8 end end while (run and (not((desiredorientation==orientation)) and not(error))) do error=not(turtle.turnLeft()) if (orientation==9) then orientation=10 else if (orientation==10) then orientation=8 else if (orientation==8) then orientation=11 else if (orientation==11) then orientation=9 end end end end end error=not(turtle.forward()) x,y,z=gps.locate() if ((x==coord[0]) and (z==coord[2])) then run=false end end end moveToCoordinates(home) local chest chest=peripheral.find("minecraft:chest") print(chest.size()) moveToCoordinates(map[32])