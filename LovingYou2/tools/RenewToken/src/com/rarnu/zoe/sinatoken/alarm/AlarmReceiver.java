package com.rarnu.zoe.sinatoken.alarm;

import com.rarnu.zoe.sinatoken.Consts;
import com.rarnu.zoe.sinatoken.MainActivity;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class AlarmReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		
		Intent inAuto = new Intent(context, MainActivity.class);
		inAuto.setAction(Consts.ALARM_AUTO);
		inAuto.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		context.startActivity(inAuto);
		
	}

}
