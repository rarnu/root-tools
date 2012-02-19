package com.snda.gyue.classes;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ListView;
import android.widget.RemoteViews.RemoteView;

@RemoteView
public class WidgetListView extends ListView {

	public WidgetListView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}
	
	public WidgetListView(Context context, AttributeSet attrs) {
		super(context, attrs);
	}
	
	public WidgetListView(Context context) {
		super(context);
	}

}
