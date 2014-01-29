package com.rarnu.devlib.component;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.intf.OnPasswordInputListener;

public class PasswordView extends RelativeLayout {

    View innerView;
    ImageView[] ivPwds;
    String password;
    OnPasswordInputListener passwordListener;
    private Handler hInputPwd = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (passwordListener != null) {
                    passwordListener.onPasswordInputed(password);
                }
            }
            super.handleMessage(msg);
        }
    };

    public PasswordView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public PasswordView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public PasswordView(Context context) {
        super(context);
        init();
    }

    public void setPasswordListener(OnPasswordInputListener listener) {
        this.passwordListener = listener;
    }

    private void init() {
        innerView = inflate(getContext(), R.layout.password_view, null);
        LayoutParams lp = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT);
        innerView.setLayoutParams(lp);
        addView(innerView);

        ivPwds = new ImageView[4];
        ivPwds[0] = (ImageView) innerView.findViewById(R.id.ivPwd1);
        ivPwds[1] = (ImageView) innerView.findViewById(R.id.ivPwd2);
        ivPwds[2] = (ImageView) innerView.findViewById(R.id.ivPwd3);
        ivPwds[3] = (ImageView) innerView.findViewById(R.id.ivPwd4);
    }

    public void setPassword(String password) {
        this.password = password;
        for (ImageView iv : ivPwds) {
            iv.setVisibility(View.GONE);
        }
        for (int i = 0; i < password.length(); i++) {
            ivPwds[i].setVisibility(View.VISIBLE);
        }

        if (password.length() == 4) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        Thread.sleep(500);
                    } catch (Exception e) {

                    }
                    hInputPwd.sendEmptyMessage(1);
                }
            }).start();


        }
    }
}
