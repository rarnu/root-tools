package com.snda.root.remoteadb;

public class Commands {
	public static final String GET_PORT = "getprop service.adb.tcp.port";
	public static final String SET_PORT = "setprop service.adb.tcp.port 5555";
	public static final String SET_PORT_USB = "setprop service.adb.tcp.port -1";
	
	public static final String STOP_ADBD = "stop adbd";
	public static final String START_ADBD = "start adbd";
}
