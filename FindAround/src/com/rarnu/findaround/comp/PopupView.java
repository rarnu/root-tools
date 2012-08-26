package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.ViewGroup;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.baidu.mapapi.GeoPoint;
import com.rarnu.findaround.R;
import com.rarnu.findaround.common.UIUtils;

public class PopupView extends RelativeLayout {

	private TextView tvRouteStep;
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
				UIUtils.dipToPx(48)));
		tvRouteStep = (TextView) findViewById(R.id.tvRouteStep);
	}

	public void setText(String name) {
		tvRouteStep.setText(name);
	}

	public void setGeoPoint(GeoPoint geo) {
		this.geo = geo;
	}

	public GeoPoint getGeoPoint() {
		return geo;
	}

}
