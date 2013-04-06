package com.example.cmdlinedemo;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

import com.rarnu.terminal.callback.ReturnDataCallback;
import com.rarnu.terminal.session.ShellTermSession;
import com.rarnu.terminal.session.TermSession;

public class MainActivity extends Activity implements ReturnDataCallback {

	TermSession session;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		
		session = new ShellTermSession("");
		session.setDefaultUTF8Mode(true);
		session.setReturnDataCallback(this);
		
		session.initializeEmulator(1, 1);
		session.write("su\rgetevent\r");
	}
	
	@Override
	protected void onDestroy() {
		session.finish();
		super.onDestroy();
	}

	@Override
	public void onReceiveData(String data) {
		Log.e("onReceiveData", data);
		
	}


}
