package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.wifi.WifiConfiguration;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.FileTransfeAdapter;
import com.rarnu.tools.root.common.FileTransferItem;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.SelectAPActivity;
import com.rarnu.tools.root.fragmentactivity.SelectSendFileActivity;
import com.rarnu.utils.WifiUtils;
import com.rarnu.utils.socket.FileSocketClient;
import com.rarnu.utils.socket.FileSocketServer;
import com.rarnu.utils.socket.SocketClientCallback;
import com.rarnu.utils.socket.SocketServerCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class FileTransferFragment extends BaseFragment implements View.OnClickListener, SocketServerCallback, SocketClientCallback {

    private static final String AP_PASSWORD = "roottools";
    private static final String AP_NAME = "RootTools-%d";
    private static final int PORT = 8821;
    private static final String AP_IP = "192.168.43.1";

    Button btnSendFile, btnReceiveFile;
    RelativeLayout laySend, layRecv;
    MenuItem miReturn;
    WifiUtils wifi;
    FileSocketServer fileServer;
    FileSocketClient fileClient;

    FileTransfeAdapter adapter;
    List<FileTransferItem> list;
    private Handler hProgressChanged = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (getActivity() != null) {
                switch (msg.what) {
                    case 0:
                        FileTransferItem item = (FileTransferItem) msg.obj;
                        tvWaitFile.setVisibility(View.GONE);
                        list.add(item);
                        adapter.setNewList(list);
                        break;
                    case 1:
                        updateTransferFinish(msg.arg1);
                        break;
                    case 2:
                        updateTransferList(msg.arg1, msg.arg2);
                        break;
                }
            }
            super.handleMessage(msg);
        }
    };
    // status
    int randomApId = 0;
    int currentWifiId = -1;
    int apWifiId = -1;
    // send
    DataProgressBar progressSend;
    ListView lvSend;
    // recv
    DataProgressBar progressRecv;
    ListView lvRecv;
    TextView tvWaitFile;
    boolean inOperating = false;

    @Override
    public int getBarTitle() {
        return R.string.func_file_transfer;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_file_transfer_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        btnSendFile = (Button) innerView.findViewById(R.id.btnSendFile);
        btnReceiveFile = (Button) innerView.findViewById(R.id.btnReceiveFile);
        laySend = (RelativeLayout) innerView.findViewById(R.id.laySend);
        layRecv = (RelativeLayout) innerView.findViewById(R.id.layRecv);
        laySend.setVisibility(View.GONE);
        layRecv.setVisibility(View.GONE);

        // send
        progressSend = (DataProgressBar) innerView.findViewById(R.id.progressSend);
        progressSend.setAppName(getString(R.string.ft_send_hint));
        lvSend = (ListView) innerView.findViewById(R.id.lvSend);

        // recv
        progressRecv = (DataProgressBar) innerView.findViewById(R.id.progressReceive);
        lvRecv = (ListView) innerView.findViewById(R.id.lvRecv);
        tvWaitFile = (TextView) innerView.findViewById(R.id.tvWaitFile);

        wifi = new WifiUtils(getActivity());
        list = new ArrayList<FileTransferItem>();
        adapter = new FileTransfeAdapter(getActivity(), list);
        lvRecv.setAdapter(adapter);
        lvSend.setAdapter(adapter);
    }

    @Override
    public void initEvents() {
        btnSendFile.setOnClickListener(this);
        btnReceiveFile.setOnClickListener(this);

    }

    @Override
    public void initLogic() {
        backupStatus();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_file_transfer;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miReturn = menu.add(0, MenuItemIds.MENU_RETURN, 99, R.string.back);
        miReturn.setIcon(android.R.drawable.ic_menu_revert);
        miReturn.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miReturn.setVisible(false);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_RETURN:
                doReturn();
                break;
        }
        return true;
    }

    private void doReturn() {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.ft_return_confirm)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        resetStatus();
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void backupStatus() {
        currentWifiId = wifi.getNetWordId();
    }

    private void resetStatus() {
        try {
            if (fileClient != null) {
                fileClient.stop();
            }
        } catch (Exception e) {

        }

        try {
            if (fileServer != null) {
                fileServer.stopListen();
            }
        } catch (Exception e) {

        }

        if (randomApId != 0) {
            wifi.createWifiAp(String.format(AP_NAME, randomApId), AP_PASSWORD, false);
            randomApId = 0;
            if (currentWifiId != -1) {
                wifi.openWifi();
                wifi.connectWifi(currentWifiId);
            }
        }

        if (apWifiId != -1) {
            wifi.disConnectionWifi(apWifiId);
            wifi.removeWifi(apWifiId);
            apWifiId = -1;
        }

        miReturn.setVisible(false);
        btnReceiveFile.setVisibility(View.VISIBLE);
        btnSendFile.setVisibility(View.VISIBLE);
        layRecv.setVisibility(View.GONE);
        laySend.setVisibility(View.GONE);
        tvWaitFile.setVisibility(View.VISIBLE);
        list.clear();
        adapter.setNewList(list);
        inOperating = false;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        Bundle bn = new Bundle();
        bn.putBoolean("inOperating", inOperating);
        return bn;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSendFile:
                inOperating = true;
                wifi.openWifi();
                laySend.setVisibility(View.VISIBLE);
                miReturn.setVisible(true);
                btnReceiveFile.setVisibility(View.GONE);
                btnSendFile.setVisibility(View.GONE);
                doSelectFile();
                break;
            case R.id.btnReceiveFile:
                inOperating = true;
                layRecv.setVisibility(View.VISIBLE);
                tvWaitFile.setVisibility(View.VISIBLE);
                miReturn.setVisible(true);
                btnReceiveFile.setVisibility(View.GONE);
                btnSendFile.setVisibility(View.GONE);
                initReceiver();
                break;
        }
    }

    private void initReceiver() {
        randomApId = new Random(System.currentTimeMillis()).nextInt(65536);
        progressRecv.setAppName(getString(R.string.ft_ap_id, randomApId));
        wifi.createWifiAp(String.format(AP_NAME, randomApId), AP_PASSWORD, true);
        fileServer = new FileSocketServer(this, PORT, "/sdcard/");
        fileServer.startListen();
    }

    @Override
    public void onCallback(String msg) {
        Log.e("onCallback", msg);
    }

    @Override
    public void onError(String msg) {
        Log.e("onError", msg);
    }

    @Override
    public void onSendFile(int id, String fileName, long total, long progress, int status) {
        switch (status) {
            case 0:
                Message msgStart = new Message();
                msgStart.what = 0;
                FileTransferItem item = new FileTransferItem();
                item.id = id;
                item.fileName = fileName.substring(fileName.lastIndexOf("/") + 1);
                item.total = total;
                item.progress = progress;
                item.inProgress = true;
                msgStart.obj = item;
                hProgressChanged.sendMessage(msgStart);
                break;
            case 1:
                Message msgEnd = new Message();
                msgEnd.what = 1;
                msgEnd.arg1 = id;
                hProgressChanged.sendMessage(msgEnd);
                break;
            case 2:
                Message msgProg = new Message();
                msgProg.what = 2;
                msgProg.arg1 = id;
                msgProg.arg2 = (int) progress;
                hProgressChanged.sendMessage(msgProg);
                break;
        }
    }

    @Override
    public void onReceiveMessage(String msg) {

    }

    @Override
    public void onReceiveFile(int id, String fileName, long total, long progress, int status) {
        switch (status) {
            case 0:
                Message msgStart = new Message();
                msgStart.what = 0;
                FileTransferItem item = new FileTransferItem();
                item.id = id;
                item.fileName = fileName.substring(fileName.lastIndexOf("/") + 1);
                item.total = total;
                item.progress = progress;
                item.inProgress = true;
                msgStart.obj = item;
                hProgressChanged.sendMessage(msgStart);
                break;
            case 1:
                Message msgEnd = new Message();
                msgEnd.what = 1;
                msgEnd.arg1 = id;
                hProgressChanged.sendMessage(msgEnd);
                break;
            case 2:
                Message msgProg = new Message();
                msgProg.what = 2;
                msgProg.arg1 = id;
                msgProg.arg2 = (int) progress;
                hProgressChanged.sendMessage(msgProg);

                break;
        }
    }

    private void updateTransferList(int id, long progress) {
        for (FileTransferItem item : list) {
            if (item.id == id) {
                item.progress = progress;
                adapter.notifyDataSetChanged();
                break;
            }
        }
    }

    private void updateTransferFinish(int id) {
        for (FileTransferItem item : list) {
            if (item.id == id) {
                item.inProgress = false;
                adapter.notifyDataSetChanged();
                break;
            }
        }
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            switch (requestCode) {
                case RTConsts.REQCODE_SENDFILE_SELECT:
                    resetStatus();
                    break;
                case RTConsts.REQCODE_AP_SELECT:
                    doSelectFile();
                    break;
            }
            return;
        }
        switch (requestCode) {
            case RTConsts.REQCODE_SENDFILE_SELECT:
                String filePath = data.getStringExtra("path");
                doSelectAP(filePath);
                break;
            case RTConsts.REQCODE_AP_SELECT:
                String file = data.getStringExtra("filePath");
                String ssid = data.getStringExtra("id");
                doSendFile(file, ssid);
                break;
        }
    }

    private void doSelectFile() {
        Intent inSelect = new Intent(getActivity(), SelectSendFileActivity.class);
        startActivityForResult(inSelect, RTConsts.REQCODE_SENDFILE_SELECT);
    }

    private void doSelectAP(String filePath) {
        Intent inAp = new Intent(getActivity(), SelectAPActivity.class);
        inAp.putExtra("filePath", filePath);
        startActivityForResult(inAp, RTConsts.REQCODE_AP_SELECT);
    }

    private void doSendFile(final String filePath, final String ssid) {
        WifiConfiguration wc = wifi.createWifiInfo(ssid, AP_PASSWORD, 3);
        apWifiId = wifi.addNetWork(wc);

        if (apWifiId != -1) {
            // TODO: must run after received a notification
            fileClient = new FileSocketClient(this, AP_IP, PORT);
            fileClient.sendFile(filePath);
        }
    }
}
