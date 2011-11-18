package com.snda.root.media;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.widget.Toast;

public class MainActivity extends Activity {
	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		allScan();
		Toast.makeText(this, R.string.scan, Toast.LENGTH_LONG).show();
		finish();
	}

	public void allScan() {
		sendBroadcast(new Intent(Intent.ACTION_MEDIA_MOUNTED, Uri
				.parse("file://" + Environment.getExternalStorageDirectory())));
	}
}