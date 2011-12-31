package com.snda.root.bcm;

import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.snda.root.bcm.adapter.ReceiverAdapter;

public class PackageInfoActivity extends Activity {

	ImageView ivAppIcon;
	TextView tvAppName, tvAppPackage;

	ListView lvReceiver;
	ReceiverAdapter adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.packageinfo);

		ivAppIcon = (ImageView) findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) findViewById(R.id.tvAppPackage);

		lvReceiver = (ListView) findViewById(R.id.lvReceiver);

		ivAppIcon.setBackgroundDrawable(GlobalInstance.currentPackageInfo.icon);
		tvAppName.setText(GlobalInstance.currentPackageInfo.label);
		tvAppPackage
				.setText(GlobalInstance.currentPackageInfo.pack.packageName);

		tvAppName
				.setTextColor(GlobalInstance.currentPackageInfo.isSytemApp ? Color.BLUE
						: Color.BLACK);

		fillReceiverList();

	}

	private void fillReceiverList() {
		// lvReceiver
		adapter = new ReceiverAdapter(getLayoutInflater(),
				GlobalInstance.currentPackageInfo.pack.receivers);
		lvReceiver.setAdapter(adapter);

	}
}
