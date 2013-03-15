package com.rarnu.devlib.component.event;

public interface OnPageChangeListener {
	public void onPageScrolled(int position, float positionOffset,
			int positionOffsetPixels);

	public void onPageSelected(int position);

}