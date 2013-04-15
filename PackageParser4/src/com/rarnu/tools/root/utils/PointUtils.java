package com.rarnu.tools.root.utils;

import android.graphics.Point;

public class PointUtils {


	private static int X_MIN = 0;
	private static int Y_MIN = 0;
	private static int X_MAX = 1535;
	private static int Y_MAX = 2559;

	private static int WIDTH = 720;
	private static int HEIGHT = 1280;

	private static int LAST_X = 0;
	private static int LAST_Y = 0;
	
	/**
	 * use getevent -p for these parameters
	 * @param xmin
	 * @param ymin
	 * @param xmax
	 * @param ymax
	 * @param width
	 * @param height
	 */
	public static void init(int xmin, int ymin, int xmax, int ymax, int width, int height) {
		X_MIN = xmin;
		Y_MIN = ymin;
		X_MAX = xmax;
		Y_MAX = ymax;
		WIDTH = width;
		HEIGHT = height;
	}

	public static Point getRealPoint(String logStr) {
		
		String[] logs = logStr.split("\r\n");
		
		Point ret = new Point(-1, -1);
		
		for (String s: logs) {
			if (s.length() == 18) {

				if (s.startsWith("0003 0035 ")
						|| s.startsWith("0003 0036 ")) {

					String[] ss = s.split(" ");

					int value = Integer.valueOf(ss[2], 16);

					if (ss[1].equals("0035")) {
						value = (value - X_MIN) * WIDTH / (X_MAX - X_MIN);
						ret.x = value;
						LAST_X = value;
						ret.y = LAST_Y;
					} else if (ss[1].equals("0036")) {
						value = (value - Y_MIN) * HEIGHT / (Y_MAX - Y_MIN);
						ret.y = value;
						LAST_Y = value;
						ret.x = LAST_X;
					}
					
				}
				if (ret.x != -1 && ret.y != -1) {
					break;
				}
			}
		}
		return ret;
	}

}
