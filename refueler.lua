function move(direction) local error if (direction==22) then error=not(turtle.down()) else if (direction==23) then error=not(turtle.up()) else if (direction==24) then error=not(turtle.left()) else if (direction==25) then error=not(turtle.right()) else if (direction==26) then error=not(turtle.forward()) else if (direction==27) then error=not(turtle.back()) else error=true end end end end end end return(error) end  local selectStack local selectStackTop selectStack=(function() local t1056={} return t1056 end)() selectStackTop=0 function pushSelect(index) selectStackTop=(selectStackTop+1) selectStack[selectStackTop]=turtle.getSelectedSlot() turtle.select(index) end function popSelect() turtle.select(selectStack[selectStackTop]) selectStackTop=(selectStackTop-1) end move(22) move(22) move(27) move(27) move(27) move(27) move(27) move(27) move(24) move(26) move(26) move(26) move(26) move(26) move(26) move(26) move(24) move(26) move(22) pushSelect(2) turtle.suck() popSelect() turtle.drop() move(23) move(23) move(26) move(26) move(26) move(26) move(26) move(26) move(25) move(27) move(27) move(27) move(27) move(27) move(27) move(27) move(25) move(27) move(23)