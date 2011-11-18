package com.snda.root.remoteadb;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

public class MainActivity extends Activity implements OnClickListener {
	Button btnSwitch;
	TextView tvStatus;

	boolean on_rc = false;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		btnSwitch = (Button) findViewById(R.id.btnSwitch);
		tvStatus = (TextView) findViewById(R.id.tvStatus);

		btnSwitch.setOnClickListener(this);

		if (RootUtils.hasRoot() == 0) {
			tvStatus.setText(R.string.str_no_root);
			btnSwitch.setEnabled(false);
			return;
		}
		if (!RemoteAdbd.isWifiActive(this)) {
			tvStatus.setText(R.string.str_no_wifi);
			btnSwitch.setEnabled(false);
			return;
		}
		refreshStatus();
	}

	public void refreshStatus() {
		on_rc = RemoteAdbd.isRemoteConnected();
		if (!on_rc) {
			btnSwitch.setText(R.string.str_start_adb);
			tvStatus.setText(R.string.str_via_usb);
		} else {
			btnSwitch.setText(R.string.str_stop_adb);
			tvStatus.setText(String.format(getResources().getString(
					R.string.str_via_remote), RemoteAdbd.getIpAddress()));
		}
	}

	@Override
	public void onClick(View v) {
		RemoteAdbd.switchAdbd(on_rc);
		refreshStatus();
	}
}