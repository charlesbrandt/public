# FastAPI

Nice and clean. Plenty of features built in. 

Quick example with dynamic routes:

```
from fastapi import FastAPI

app = FastAPI()


@app.get('/users/{user_id}/{user_name}')
async def get_users(user_id: int, user_name: str):
    return {"user_data": {
        "id": user_id,
        "message": "Hello, " + user_name,
    }}
```

[via](https://www.slingacademy.com/article/how-to-use-dynamic-routes-in-fastapi/)
