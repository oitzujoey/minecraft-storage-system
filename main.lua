local map map=(function() local t164={} t164[2]=(function() local t165={} t165[0]=201 t165[1]=62 t165[2]=714 return t165 end)() t164[3]=(function() local t166={} t166[0]=179 t166[1]=62 t166[2]=718 return t166 end)() t164["minecraft:coal"]=(function() local t167={} t167[0]=180 t167[1]=62 t167[2]=697 return t167 end)() t164["minecraft:netherrack"]=(function() local t168={} t168[0]=181 t168[1]=62 t168[2]=697 return t168 end)() t164["create:shaft"]=(function() local t169={} t169[0]=182 t169[1]=62 t169[2]=697 return t169 end)() t164["thermal:apatite"]=(function() local t170={} t170[0]=183 t170[1]=62 t170[2]=697 return t170 end)() return t164 end)() local home local fuel home=map[2] fuel=map[3] local orientation local desiredorientation orientation=7 local error error=false local ox local oy local oz local x local y local z local dx local dy local dz error=not(turtle.back()) ox,oy,oz=gps.locate() error=not(turtle.forward()) x,y,z=gps.locate() dx=(x-ox) dz=(z-oz) if not (dx==0) then if (dx<0) then orientation=8 else orientation=9 end end if not (dz==0) then if (dz<0) then orientation=10 else orientation=11 end end function moveToCoordinates(coord,orient) local run run=true while (run and not(error)) do x,y,z=gps.locate() if (y>coord[1]) then error=not(turtle.down()) else if (y<coord[1]) then error=not(turtle.up()) else run=false end end end run=true x,y,z=gps.locate() while (run and not(error)) do if (x==coord[0]) then if (z==coord[2]) then run=false else if (z<coord[2]) then desiredorientation=11 else desiredorientation=10 end end else if (x<coord[0]) then desiredorientation=9 else desiredorientation=8 end end while (run and (not((desiredorientation==orientation)) and not(error))) do error=not(turtle.turnLeft()) if (orientation==9) then orientation=10 else if (orientation==10) then orientation=8 else if (orientation==8) then orientation=11 else if (orientation==11) then orientation=9 end end end end end error=not(turtle.forward()) x,y,z=gps.locate() if ((x==coord[0]) and (z==coord[2])) then run=false end end run=true while (run and (not((orient==orientation)) and not(error))) do error=not(turtle.turnLeft()) if (orientation==9) then orientation=10 else if (orientation==10) then orientation=8 else if (orientation==8) then orientation=11 else if (orientation==11) then orientation=9 end end end end end end local courier_slot courier_slot=1 function sortItem() local error error=false moveToCoordinates(home,9) turtle.select(courier_slot) turtle.suck() local item local name local coord item=turtle.getItemDetail() if (nil==item) then error=true else name=item.name print(name) coord=map[name] print(coord) if (nil==coord) then error=true else moveToCoordinates(coord,10) turtle.dropUp() moveToCoordinates(home,9) end end return(error) end while not(sortItem()) do  end