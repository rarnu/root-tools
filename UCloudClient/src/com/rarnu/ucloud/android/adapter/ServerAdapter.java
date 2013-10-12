package com.rarnu.ucloud.android.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.pojo.ServerItem;

import java.util.List;

public class ServerAdapter extends BaseAdapter<ServerItem> {

    private String[] SERVER_STATE = null;
    private int[] SERVER_STATE_COLOR = null;
    private int imageHeight = 0;

    public ServerAdapter(Context context, List<ServerItem> list, int imageHeight) {
        super(context, list);
        this.imageHeight = imageHeight;
        SERVER_STATE = context.getResources().getStringArray(R.array.array_server_state);
        SERVER_STATE_COLOR = context.getResources().getIntArray(R.array.array_server_state_color);
    }

    @Override
    public String getValueText(ServerItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_server, parent, false);
        }
        ServerHolder holder = (ServerHolder) v.getTag();
        if (holder == null) {
            holder = new ServerHolder();
            holder.tvServerName = (TextView) v.findViewById(R.id.tvServerName);
            holder.tvRunningTime = (TextView) v.findViewById(R.id.tvRunningTime);
            holder.tvServerState = (TextView) v.findViewById(R.id.tvServerState);
            holder.imgLoadLine = (ImageView) v.findViewById(R.id.imgLoadLine);
            RelativeLayout.LayoutParams rllpImage = (RelativeLayout.LayoutParams) holder.imgLoadLine.getLayoutParams();
            rllpImage.height = imageHeight;
            holder.imgLoadLine.setLayoutParams(rllpImage);
            v.setTag(holder);
        }
        ServerItem item = list.get(position);
        if (item != null) {
            holder.tvServerName.setText(item.name);
            holder.tvRunningTime.setText(context.getString(R.string.server_item_running_time, item.runningTime));
            holder.tvServerState.setText(SERVER_STATE[item.state]);
            holder.tvServerState.setTextColor(SERVER_STATE_COLOR[item.state]);

        }
        return v;
    }
}
