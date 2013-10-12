package com.rarnu.ucloud.android.adapter;

import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.pojo.ChatItem;

import java.util.List;

public class ChatAdpater extends BaseAdapter<ChatItem> {
    public ChatAdpater(Context context, List<ChatItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(ChatItem item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_chat, parent, false);
        }
        ChatHolder holder = (ChatHolder) v.getTag();
        if (holder == null) {
            holder = new ChatHolder();
            holder.imgChatter = (ImageView) v.findViewById(R.id.imgChatter);
            holder.imgHead = (ImageView) v.findViewById(R.id.imgHead);
            holder.tvChatText = (TextView) v.findViewById(R.id.tvChatText);
            v.setTag(holder);
        }
        ChatItem item = list.get(position);
        if (item != null) {
            holder.tvChatText.setText(item.text);
            holder.tvChatText.setGravity(item.fromChatter ? Gravity.LEFT : Gravity.RIGHT);
            holder.imgChatter.setVisibility(item.fromChatter ? View.VISIBLE : View.GONE);
            holder.imgHead.setVisibility(item.fromChatter ? View.GONE : View.VISIBLE);
        }
        return v;
    }
}
