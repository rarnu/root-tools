package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.baidu.mapapi.GeoPoint;
import com.rarnu.tools.root.R;

public class MockBar extends RelativeLayout {

	private TextView tvMockGps, tvMockAddress;
	private Button btnMock, btnMockHistory;

	public MockBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public MockBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public MockBar(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.mock_bar, null));
		tvMockGps = (TextView) findViewById(R.id.tvMockGps);
		tvMockAddress = (TextView) findViewById(R.id.tvMockAddress);
		btnMock = (Button) findViewById(R.id.btnMock);
		btnMockHistory = (Button) findViewById(R.id.btnMockHistory);
	}

	public Button getMockButton() {
		return btnMock;
	}

	public Button getMockHistoryButton() {
		return btnMockHistory;
	}

	public void setMockGeo(GeoPoint geo) {
		tvMockGps.setText(String.format("Lat:%d, Lng:%d", geo.getLatitudeE6(),
				geo.getLongitudeE6()));
	}

	public void setMockAddress(String addr) {
		tvMockAddress.setText(addr);
	}

}
