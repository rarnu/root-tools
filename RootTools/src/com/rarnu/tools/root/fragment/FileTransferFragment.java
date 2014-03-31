package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.SelectSendFileActivity;
import com.rarnu.tools.root.fragmentactivity.SysappSelectApkActivity;
import com.rarnu.utils.WifiUtils;
import com.rarnu.utils.socket.FileSocketClient;
import com.rarnu.utils.socket.FileSocketServer;
import com.rarnu.utils.socket.SocketClientCallback;
import com.rarnu.utils.socket.SocketServerCallback;

import java.util.Random;

public class FileTransferFragment extends BaseFragment implements View.OnClickListener, SocketServerCallback, SocketClientCallback {

    private static final String AP_PASSWORD = "roottools";
    private static final String AP_NAME = "RootTools-%d";
    private static final int PORT = 8821;
    private static final String AP_IP = "192.168.43.1";

    Button btnSendFile, btnReceiveFile;
    RelativeLayout laySend, layRecv;
    MenuItem miReturn;
    MenuItem itemAdd;
    WifiUtils wifi;
    FileSocketServer fileServer;
    FileSocketClient fileClient;

    // status
    int randomApId = 0;
    int currentWifiId = -1;

    // send
    DataProgressBar progressSend;
    ListView lvSend;

    // recv
    DataProgressBar progressRecv;
    ListView lvRecv;

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

        wifi = new WifiUtils(getActivity());
        fileServer = new FileSocketServer(this, PORT, "/sdcard/");
        fileClient = new FileSocketClient(this, AP_IP, PORT);
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

        itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 98, R.string.add);
        itemAdd.setIcon(R.drawable.ic_menu_add);
        itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemAdd.setVisible(false);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_RETURN:
                doReturn();
                break;
            case MenuItemIds.MENU_ADD:
                Intent inSelect = new Intent(getActivity(), SelectSendFileActivity.class);
                startActivityForResult(inSelect, RTConsts.REQCODE_SENDFILE_SELECT);
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
        // TODO: backup status
        currentWifiId = wifi.getNetWordId();
    }

    private void resetStatus() {
        // TODO: reset status
        if (randomApId != 0) {
            wifi.createWifiAp(String.format(AP_NAME, randomApId), AP_PASSWORD, false);
            randomApId = 0;
            if (currentWifiId != -1) {
                wifi.openWifi();
                wifi.connectWifi(currentWifiId);
            }
        }
        fileServer.stopListen();
        fileClient.stop();

        miReturn.setVisible(false);
        itemAdd.setVisible(false);
        btnReceiveFile.setVisibility(View.VISIBLE);
        btnSendFile.setVisibility(View.VISIBLE);
        layRecv.setVisibility(View.GONE);
        laySend.setVisibility(View.GONE);
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
                laySend.setVisibility(View.VISIBLE);
                miReturn.setVisible(true);
                itemAdd.setVisible(true);
                btnReceiveFile.setVisibility(View.GONE);
                btnSendFile.setVisibility(View.GONE);
                break;
            case R.id.btnReceiveFile:
                inOperating = true;
                layRecv.setVisibility(View.VISIBLE);
                miReturn.setVisible(true);
                btnReceiveFile.setVisibility(View.GONE);
                btnSendFile.setVisibility(View.GONE);
                initReceiver();
                break;
        }
    }

    private void initReceiver() {
        randomApId = new Random(65535).nextInt();
        progressRecv.setAppName(getString(R.string.ft_ap_id, randomApId));
        wifi.createWifiAp(String.format(AP_NAME, randomApId), AP_PASSWORD, true);
    }

    @Override
    public void onCallback(String msg) {

    }

    @Override
    public void onError(String msg) {

    }

    @Override
    public void onSendFile(String fileName, long total, long progress, int status) {

    }

    @Override
    public void onReceiveMessage(String msg) {

    }

    @Override
    public void onReceiveFile(String fileName, long total, long progress, int status) {

    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case RTConsts.REQCODE_SENDFILE_SELECT:
                String filePath = data.getStringExtra("path");
                // TODO send file
                break;
        }
    }
}
