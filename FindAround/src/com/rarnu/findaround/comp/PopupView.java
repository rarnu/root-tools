package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.baidu.mapapi.GeoPoint;
import com.rarnu.findaround.R;

public class PopupView extends RelativeLayout {

	private TextView tvName, tvAddress;
	private GeoPoint geo;

	public PopupView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public PopupView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public PopupView(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.popup_view, null));
		setLayoutParams(new ViewGroup.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT));
		tvName = (TextView) findViewById(R.id.tvName);
		tvAddress = (TextView) findViewById(R.id.tvAddress);
	}

	public void setName(String name) {
		tvName.setText(name);
	}

	public void setAddress(String addr) {
		tvAddress.setText(addr);
	}

	public void setGeoPoint(GeoPoint geo) {
		this.geo = geo;
	}

	public GeoPoint getGeoPoint() {
		return geo;
	}

}
