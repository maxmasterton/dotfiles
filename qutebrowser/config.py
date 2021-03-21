import dracula.draw

config.load_autoconfig()
dracula.draw.blood(c,{
    'spacing':{
        'vertical':6,
        'horizontal':8
    }
})

#c.aliases = {
#        'q': 'quit',
#        'w': 'session-save',
#        'wq': 'quit --save'
#        }

config.set("content.cookies.accept", "all", "chrome-devtools://*")
config.set("content.cookies.accept", "all", "devtools://*")
