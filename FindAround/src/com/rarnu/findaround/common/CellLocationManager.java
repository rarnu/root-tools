package com.rarnu.findaround.common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiManager;
import android.os.Handler;
import android.os.Message;
import android.telephony.CellLocation;
import android.util.Log;
import android.widget.Toast;

public abstract class CellLocationManager {
	public static int CHECK_INTERVAL = 15000;
	public static boolean ENABLE_WIFI = true;
	private static boolean IS_DEBUG = false;
	private static final int STATE_COLLECTING = 2;
	private static final int STATE_IDLE = 0;
	private static final int STATE_READY = 1;
	private static final int STATE_SENDING = 3;
	private static final int MESSAGE_INITIALIZE = 1;
	private static final int MESSAGE_COLLECTING_CELL = 2;
	private static final int MESSAGE_COLLECTING_WIFI = 5;
	private static final int MESSAGE_BEFORE_FINISH = 10;
	private int accuracy;
	private int bid;
	private CellInfoManager cellInfoManager;
	private Context context;
	private boolean disableWifiAfterScan;
	private int[] aryGsmCells;
	private double latitude;
	private double longitude;
	private MyLooper looper;
	private boolean paused;
	private final BroadcastReceiver receiver;
	private long startScanTimestamp;
	private int state;
	private Task task;
	private long timestamp;
	private boolean waiting4WifiEnable;
	private WifiInfoManager wifiManager;

	public CellLocationManager(Context context,
			CellInfoManager cellinfomanager, WifiInfoManager wifiinfomanager) {
		receiver = new CellLocationManagerBroadcastReceiver();
		this.context = context.getApplicationContext();
		cellInfoManager = cellinfomanager;
		wifiManager = wifiinfomanager;
	}

	private void debug(Object paramObject) {
		if (IS_DEBUG) {
			System.out.println(paramObject);
			String str = String.valueOf(paramObject);
			Toast.makeText(this.context, str, Toast.LENGTH_SHORT).show();
		}
	}

	public int accuracy() {
		return this.accuracy;
	}

	public double latitude() {
		return this.latitude;
	}

	public double longitude() {
		return this.longitude;
	}

	public abstract void onLocationChanged();

	public void pause() {
		if (state > 0 && !paused) {
			looper.removeMessages(MESSAGE_BEFORE_FINISH);
			paused = true;
		}
	}

	public void requestUpdate() {
		if (state != STATE_READY) {
			return;
		}
		boolean bStartScanSuccessful = false;
		CellLocation.requestLocationUpdate();
		state = STATE_COLLECTING;
		looper.sendEmptyMessage(MESSAGE_INITIALIZE);
		if (wifiManager.wifiManager().isWifiEnabled()) {
			bStartScanSuccessful = wifiManager.wifiManager().startScan();
			waiting4WifiEnable = false;
		} else {
			startScanTimestamp = System.currentTimeMillis();
			if (!ENABLE_WIFI || !wifiManager.wifiManager().setWifiEnabled(true)) {
				int nDelay = 0;
				if (!bStartScanSuccessful)
					nDelay = 8000;
				looper.sendEmptyMessageDelayed(MESSAGE_COLLECTING_WIFI, nDelay);
				debug("CELL UPDATE");
			} else {
				waiting4WifiEnable = true;
			}
		}
	}

	public void resume() {
		if (state > 0 && paused) {
			paused = false;
			looper.removeMessages(MESSAGE_BEFORE_FINISH);
			looper.sendEmptyMessage(MESSAGE_BEFORE_FINISH);
		}
	}

	public void start() {
		if (state <= STATE_IDLE) {
			Log.i("CellLocationManager", "Starting...");
			context.registerReceiver(receiver, new IntentFilter(
					WifiManager.SCAN_RESULTS_AVAILABLE_ACTION));
			context.registerReceiver(receiver, new IntentFilter(
					WifiManager.WIFI_STATE_CHANGED_ACTION));
			looper = new MyLooper();
			state = STATE_READY;
			paused = false;
			waiting4WifiEnable = false;
			disableWifiAfterScan = false;
			debug("CELL LOCATION START");
			requestUpdate();
		}
	}

	public void stop() {
		if (state > STATE_IDLE) {
			context.unregisterReceiver(receiver);
			debug("CELL LOCATION STOP");
			looper = null;
			state = STATE_IDLE;
			if (disableWifiAfterScan) {
				disableWifiAfterScan = false;
				wifiManager.wifiManager().setWifiEnabled(false);
			}
		}
	}

	public long timestamp() {
		return this.timestamp;
	}

	protected boolean isConnectedWithInternet() {
		ConnectivityManager conManager = (ConnectivityManager) context
				.getSystemService(Context.CONNECTIVITY_SERVICE);
		NetworkInfo networkInfo = conManager.getActiveNetworkInfo();
		if (networkInfo != null) {
			return networkInfo.isAvailable();
		}
		return false;
	}

	private class MyLooper extends Handler {
		private float fCellScore;
		private JSONArray objCellTowersJson;

		public void handleMessage(Message paramMessage) {
			if (CellLocationManager.this.looper != this)
				return;
			boolean flag = true;
			switch (paramMessage.what) {
			default:
				break;
			case MESSAGE_INITIALIZE:
				this.objCellTowersJson = null;
				this.fCellScore = 1.401298E-045F;
			case MESSAGE_COLLECTING_CELL:
				if (CellLocationManager.this.state != CellLocationManager.STATE_COLLECTING)
					break;
				JSONArray objCellTowers = CellLocationManager.this.cellInfoManager
						.cellTowers();
				float fCellScore = CellLocationManager.this.cellInfoManager
						.score();
				if (objCellTowers != null) {
					float fCurrentCellScore = this.fCellScore;
					if (fCellScore > fCurrentCellScore) {
						this.objCellTowersJson = objCellTowers;
						this.fCellScore = fCellScore;
					}
				}
				this.sendEmptyMessageDelayed(MESSAGE_COLLECTING_CELL, 600L);
				break;
			case MESSAGE_COLLECTING_WIFI:
				if (CellLocationManager.this.state != CellLocationManager.STATE_COLLECTING)
					break;
				this.removeMessages(MESSAGE_COLLECTING_CELL);
				this.removeMessages(MESSAGE_BEFORE_FINISH);
				// if (CellLocationManager.this.disableWifiAfterScan &&
				// CellLocationManager.this.wifiManager.wifiManager().setWifiEnabled(true))
				// CellLocationManager.this.disableWifiAfterScan = false;
				CellLocationManager.this.state = CellLocationManager.STATE_SENDING;
				if (CellLocationManager.this.task != null)
					CellLocationManager.this.task.cancel(true);
				int[] aryCell = null;
				if (CellLocationManager.this.cellInfoManager.isGsm())
					aryCell = CellLocationManager.this.cellInfoManager
							.dumpCells();
				int nBid = CellLocationManager.this.cellInfoManager.bid();
				CellLocationManager.this.task = new CellLocationManager.Task(
						aryCell, nBid);
				JSONArray[] aryJsonArray = new JSONArray[2];
				aryJsonArray[0] = this.objCellTowersJson;
				aryJsonArray[1] = CellLocationManager.this.wifiManager
						.wifiTowers();
				if (this.objCellTowersJson != null)
					Log.i("CellTownerJSON", this.objCellTowersJson.toString());
				if (aryJsonArray[1] != null)
					Log.i("WIFITownerJSON", aryJsonArray[1].toString());
				CellLocationManager.this.debug("Post json");
				CellLocationManager.this.task.execute(aryJsonArray);
				break;
			case MESSAGE_BEFORE_FINISH:
				if (CellLocationManager.this.state != CellLocationManager.STATE_READY
						|| CellLocationManager.this.paused)
					break;
				// L7
				if (CellLocationManager.this.disableWifiAfterScan
						&& CellLocationManager.this.wifiManager.wifiManager()
								.setWifiEnabled(false))
					CellLocationManager.this.disableWifiAfterScan = false;
				if (!CellLocationManager.this.cellInfoManager.isGsm()) {
					// L9
					if (CellLocationManager.this.bid == CellLocationManager.this.cellInfoManager
							.bid()) {
						flag = true;
					} else {
						flag = false;
					}
					// L14
					if (flag) {
						requestUpdate();
					} else {
						this.sendEmptyMessageDelayed(10,
								CellLocationManager.CHECK_INTERVAL);
					}
				} else {
					// L8
					if (CellLocationManager.this.aryGsmCells == null
							|| CellLocationManager.this.aryGsmCells.length == 0) {
						// L10
						flag = true;
					} else {
						int[] aryCells = CellLocationManager.this.cellInfoManager
								.dumpCells();
						if (aryCells != null && aryCells.length != 0) {
							// L13
							int nFirstCellId = CellLocationManager.this.aryGsmCells[0];
							if (nFirstCellId == aryCells[0]) {
								// L16
								int cellLength = CellLocationManager.this.aryGsmCells.length / 2;
								List<Integer> arraylist = new ArrayList<Integer>(
										cellLength);
								List<Integer> arraylist1 = new ArrayList<Integer>(
										aryCells.length / 2);
								int nIndex = 0;
								int nGSMCellLength = CellLocationManager.this.aryGsmCells.length;
								while (nIndex < nGSMCellLength) {
									// goto L18
									arraylist
											.add(CellLocationManager.this.aryGsmCells[nIndex]);
									nIndex += 2;
								}
								// goto L17
								nIndex = 0;
								while (nIndex < aryCells.length) {
									// goto L20
									arraylist1.add(aryCells[nIndex]);
									nIndex += 2;
								}
								// goto L19
								int nCounter = 0;
								for (Iterator<Integer> iterator = arraylist
										.iterator(); iterator.hasNext();) {
									// goto L22
									if (arraylist1.contains(iterator.next()))
										nCounter++;
								}
								// goto L21
								int k4 = arraylist.size() - nCounter;
								int l4 = arraylist1.size() - nCounter;
								if (k4 + l4 > nCounter)
									flag = true;
								else
									flag = false;
								if (flag) {
									StringBuilder stringbuilder = new StringBuilder(
											k4).append(" + ");
									stringbuilder.append(l4).append(" > ");
									stringbuilder.append(nCounter);
									CellLocationManager.this
											.debug(stringbuilder.toString());
								}
								break;
							} else {
								// L15
								flag = true;
								CellLocationManager.this
										.debug("PRIMARY CELL CHANGED");
								// goto L14
								if (flag) {
									requestUpdate();
								} else {
									this.sendEmptyMessageDelayed(
											MESSAGE_BEFORE_FINISH,
											CellLocationManager.CHECK_INTERVAL);
								}
							}
						} else {
							// L12
							flag = true;
							// goto L14
							if (flag) {
								requestUpdate();
							} else {
								this.sendEmptyMessageDelayed(
										MESSAGE_BEFORE_FINISH,
										CellLocationManager.CHECK_INTERVAL);
							}
						}
					}
				}
			}
		}
	}

	class Task extends UserTask<JSONArray, Void, Void> {
		int accuracy;
		int bid;
		int[] cells;
		double lat;
		double lng;
		long time;

		public Task(int[] aryCell, int bid) {
			this.time = System.currentTimeMillis();
			this.cells = aryCell;
			this.bid = bid;
		}

		public Void doInBackground(JSONArray[] paramArrayOfJSONArray) {
			try {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("version", "1.1.0");
				jsonObject.put("host", "maps.google.com");
				jsonObject.put("address_language", "zh_CN");
				jsonObject.put("request_address", true);
				jsonObject.put("radio_type", "gsm");
				jsonObject.put("carrier", "HTC");
				JSONArray cellJson = paramArrayOfJSONArray[0];
				jsonObject.put("cell_towers", cellJson);
				JSONArray wifiJson = paramArrayOfJSONArray[1];
				jsonObject.put("wifi_towers", wifiJson);
				DefaultHttpClient localDefaultHttpClient = new DefaultHttpClient();
				HttpPost localHttpPost = new HttpPost(
						"http://www.google.com/loc/json");
				String strJson = jsonObject.toString();
				StringEntity objJsonEntity = new StringEntity(strJson);
				localHttpPost.setEntity(objJsonEntity);
				HttpResponse objResponse = localDefaultHttpClient
						.execute(localHttpPost);
				int nStateCode = objResponse.getStatusLine().getStatusCode();
				HttpEntity httpEntity = objResponse.getEntity();
				byte[] arrayOfByte = null;
				if (nStateCode / 100 == 2)
					arrayOfByte = EntityUtils.toByteArray(httpEntity);
				httpEntity.consumeContent();
				String strResponse = new String(arrayOfByte, "UTF-8");
				jsonObject = new JSONObject(strResponse);
				this.lat = jsonObject.getJSONObject("location").getDouble(
						"latitude");
				this.lng = jsonObject.getJSONObject("location").getDouble(
						"longitude");
				this.accuracy = jsonObject.getJSONObject("location").getInt(
						"accuracy");
				;
			} catch (Exception localException) {
				return null;
			}
			return null;
		}

		public void onPostExecute(Void paramVoid) {
			if (CellLocationManager.this.state != CellLocationManager.STATE_SENDING
					|| CellLocationManager.this.task != this)
				return;
			if ((this.lat != 0.0D) && (this.lng != 0.0D)) {
				CellLocationManager.this.timestamp = this.time;
				CellLocationManager.this.latitude = this.lat;
				CellLocationManager.this.longitude = this.lng;
				CellLocationManager.this.accuracy = this.accuracy;
				CellLocationManager.this.aryGsmCells = this.cells;
				CellLocationManager.this.bid = this.bid;
				StringBuilder sb = new StringBuilder("CELL LOCATION DONE: (");
				sb.append(this.lat).append(",").append(this.lng).append(")");
				CellLocationManager.this.debug(sb.toString());
				CellLocationManager.this.state = STATE_READY;
				CellLocationManager.this.looper.sendEmptyMessageDelayed(
						MESSAGE_BEFORE_FINISH,
						CellLocationManager.CHECK_INTERVAL);
				CellLocationManager.this.onLocationChanged();
			} else {
				CellLocationManager.this.task = null;
				CellLocationManager.this.state = CellLocationManager.STATE_READY;
				CellLocationManager.this.looper.sendEmptyMessageDelayed(
						MESSAGE_BEFORE_FINISH, 5000L);
			}
		}
	}

	private class CellLocationManagerBroadcastReceiver extends
			BroadcastReceiver {
		@Override
		public void onReceive(Context arg0, Intent intent) {
			// access$0 state
			// 1 debug
			// access$2 loop
			// 3 startScanTimestamp
			// 4 disableWifiAfterScan
			// 5 wifimanager
			if (CellLocationManager.this.state != CellLocationManager.STATE_COLLECTING)
				return;
			String s = intent.getAction();
			if (WifiManager.SCAN_RESULTS_AVAILABLE_ACTION.equals(s)) { // goto
																		// _L4;
																		// else
																		// goto
																		// _L3
				// _L3:
				CellLocationManager.this.debug("WIFI SCAN COMPLETE");
				CellLocationManager.this.looper
						.removeMessages(MESSAGE_COLLECTING_WIFI);
				long lInterval = System.currentTimeMillis()
						- CellLocationManager.this.startScanTimestamp;
				if (lInterval > 4000L)
					CellLocationManager.this.looper.sendEmptyMessageDelayed(
							MESSAGE_COLLECTING_WIFI, 4000L);
				else
					CellLocationManager.this.looper
							.sendEmptyMessage(MESSAGE_COLLECTING_WIFI);
			} else {
				// _L4:
				if (!CellLocationManager.this.waiting4WifiEnable)
					return;
				String s1 = intent.getAction();
				if (!WifiManager.WIFI_STATE_CHANGED_ACTION.equals(s1))
					return;
				int wifiState = intent.getIntExtra(
						WifiManager.EXTRA_WIFI_STATE, 4);
				// _L5:
				if (wifiState == WifiManager.WIFI_STATE_ENABLING) {
//					boolean flag2 = CellLocationManager.this.wifiManager
//							.wifiManager().startScan();
					// _L8:
					CellLocationManager.this.disableWifiAfterScan = true;
					CellLocationManager.this.paused = false;
					// int i = flag2 ? 1 : 0;
					// int nDelay = i != 0 ? 8000 : 0;
					// CellLocationManager.this.looper.sendEmptyMessageDelayed(MESSAGE_COLLECTING_WIFI,
					// nDelay);
					CellLocationManager.this.debug("WIFI ENABLED");
				}
			}
		}
	}
}