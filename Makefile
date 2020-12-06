all:
	gfortran.exe -c mod_table.f90 mod_point.f90 mod_robot.f90 toyrobot.f90
	gfortran.exe mod_table.o mod_point.o mod_robot.o toyrobot.o -o toyrobot.exe
	rm *.o
	
clean:
	rm *.o toyrobot.exe
