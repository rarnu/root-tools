package com.snda.root.memory;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.snda.root.memory.root.RootUtils;

public class ProcessActivity extends Activity implements OnClickListener {

	ImageView ImgIcon;
	TextView tvName, tvNamespace, tvWarning;

	TextView tvPidValue, tvMemoryValue, tvUserValue;

	Button btnCancel, btnKill;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.process);

		ImgIcon = (ImageView) findViewById(R.id.ImgIcon);
		tvName = (TextView) findViewById(R.id.tvName);
		tvNamespace = (TextView) findViewById(R.id.tvNamespace);
		tvWarning = (TextView) findViewById(R.id.tvWarning);

		tvPidValue = (TextView) findViewById(R.id.tvPidValue);
		tvMemoryValue = (TextView) findViewById(R.id.tvMemoryValue);
		tvUserValue = (TextView) findViewById(R.id.tvUserValue);

		btnCancel = (Button) findViewById(R.id.btnCancel);
		btnKill = (Button) findViewById(R.id.btnKill);

		btnCancel.setOnClickListener(this);
		btnKill.setOnClickListener(this);

		showProcessInfo();

	}

	private void showProcessInfo() {
		if (Global.currentProcInfo.appInfo == null) {
			ImgIcon.setBackgroundDrawable(getResources().getDrawable(
					R.drawable.default_icon));
			tvName.setText(Global.currentProcInfo.NAME);
			tvNamespace.setText("");
		} else {
			ImgIcon.setBackgroundDrawable(Global.pm
					.getApplicationIcon(Global.currentProcInfo.appInfo));
			tvName.setText(Global.pm
					.getApplicationLabel(Global.currentProcInfo.appInfo));
			tvNamespace.setText(Global.currentProcInfo.NAME);
		}

		tvPidValue.setText(String.valueOf(Global.currentProcInfo.PID));
		tvMemoryValue.setText(String.format("%dM", Global.currentProcInfo.RSS));
		tvUserValue.setText(Global.currentProcInfo.USER);

		tvWarning
				.setVisibility(Global.currentProcInfo.USER.startsWith("app_") ? View.GONE
						: View.VISIBLE);

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnKill:

			RootUtils.runRootCommand(String.format("kill %d",
					Global.currentProcInfo.PID));
			setResult(RESULT_OK);
			finish();

			break;
		case R.id.btnCancel:
			finish();
			break;

		}

	}

}
