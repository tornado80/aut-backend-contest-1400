import string
import random
import json
import jwt
import sys



def reset_db():
    import requests
    requests.get(f'{host}/delete/')

class Judge():

    def __init__(self, host):
        self.host = host

    def _generate_unique(self, size=6, chars=string.ascii_letters + string.digits):
        return ''.join(random.choice(chars) for _ in range(size))

    def _contains_in_dict(self, list, dict):
        for el in list:
            if el not in dict.keys():
                return False
        return True

    def _assertEqual(self, val_1, val_2):
        if val_1 != val_2:
            raise Exception("Wrong answer") # or better message

    def request(self, method, url, token=None, data=None):
        import requests
        headers = {
            'Authorization': f'Bearer {token}' if token else None,
            'Content-Type': "application/json"
        }
        return requests.request(method, url, headers=headers, data=json.dumps(data))


    def __extract_jwt_payload(self, token):
        return jwt.decode(token, verify=False)

    def _get_user_id_from_token(self, token):
        jwt_payload = self.__extract_jwt_payload(token)
        if 'userId' in token:
            return jwt_payload['userId']
        else:
            return jwt_payload['user_id']

    def ـsignup(self, email="gmail@gmail.com", name="mohsen", password="12345678@"):
        url = f'{self.host}/api/v1/auth/signup/'
        return self.request('POST', url, data={"email": email, "name": name, "password": password})

    def _login(self, email="gmail@gmail.com", password="12345678@"):
        url = f'{self.host}/api/v1/auth/login/'
        return self.request('POST', url, data={"email": email, "password": password})

    def _create_group(self, token=None, name="gank of four", description= "We are here for design ..."):
        url = f'{self.host}/api/v1/groups/'
        if not token:
            res = self.ـsignup(email=f'{self._generate_unique(chars=string.ascii_letters)}@gmail.com')
            token = res.json()['token']
        return self.request('POST', url, token=token, data={"name": name, "description": description})

    def _get_groups(self, token):
        url = f'{self.host}/api/v1/groups/'
        return self.request('GET', url, token=token)

    def _get_my_group(self, token):
        url = f'{self.host}/api/v1/groups/my/'
        return self.request('GET', url, token=token)

    def _send_join_request(self, token, group_id):
        url = f'{self.host}/api/v1/join_requests/'
        return self.request('POST', url, token=token, data={"groupId": group_id})

    def _get_my_join_requests(self, token):
        url = f'{self.host}/api/v1/join_requests/'
        return self.request('GET', url, token=token)

    def _give_permission(self, token, user_id):
        url = f'{self.host}/api/v1/permissions/{user_id}/'
        return self.request('POST', url, token=token, data={})

    def _get_group_join_requests(self, token):
        url = f'{self.host}/api/v1/join_requests/group/'
        return self.request('GET', url, token=token)

    def _acccept_join_request(self, admin_token, join_request_id):
        url = f'{self.host}/api/v1/join_requests/accept/'
        return self.request('POST', url, token=admin_token, data={"joinRequestId": join_request_id})


    def is_system_up(self):
        res = self.request('GET', f'{self.host}/api/v1/blah/blah')
        self._assertEqual(res.status_code, 404)


    def test_sign_up_work_with_proper_data(self):
        res = self.ـsignup(email='hamidif@gmail.com')
        self._assertEqual(200, res.status_code)


    def test_email_is_in_jwt_token_of_signup(self):
        res = self.ـsignup(email='hamidif@gmail.com')
        self._assertEqual(200, res.status_code)
        jwt_payload = self.__extract_jwt_payload(res.json()['token'])
        self._assertEqual(True, 'email' in jwt_payload)
        self._assertEqual('hamidif@gmail.com', jwt_payload['email'])
    

    def test_user_id_is_in_jwt_token_of_signup(self):
        res = self.ـsignup(email='hamidif@gmail.com')
        self._assertEqual(200, res.status_code)
        jwt_payload = self.__extract_jwt_payload(res.json()['token'])
        self._assertEqual(True, 'userId' in jwt_payload)


    def test_login_works_with_proper_data(self):
        self.ـsignup(email='hamid@gmail.com', password="12345678@")
        res = self._login(email='hamid@gmail.com', password="12345678@")
        self._assertEqual(200, res.status_code)

    
    def test_email_is_in_jwt_token_of_login(self):
        self.ـsignup(email='hamid@gmail.com', password="12345678@")
        res = self._login(email='hamid@gmail.com', password="12345678@")
        jwt_payload = self.__extract_jwt_payload(res.json()['token'])
        self._assertEqual(200, res.status_code)
        self._assertEqual(True, 'email' in jwt_payload)
        self._assertEqual('hamid@gmail.com', jwt_payload['email'])
    
    
    def test_user_id_is_in_jwt_token_of_login(self):
        self.ـsignup(email='hamid@gmail.com', password="12345678@")
        res = self._login(email='hamid@gmail.com', password="12345678@")
        jwt_payload = self.__extract_jwt_payload(res.json()['token'])
        self._assertEqual(200, res.status_code)
        self._assertEqual(True, 'userId' in jwt_payload)    


    def test_create_group_works_for_new_user(self):
        res = self.ـsignup()
        token = res.json()['token']

        res = self._create_group(token=token)

        self._assertEqual(200, res.status_code)
        self._assertEqual(True, 'group' in res.json())
        self._assertEqual(True, 'message' in res.json())
        self._assertEqual(2, len(res.json().keys()))
        self._assertEqual(True, 'id' in res.json()['group'])


    def test_can_get_groups_after_successfully_created(self):
        res_1 = self.ـsignup(email="gmail1@gmail.com")
        res_2 = self.ـsignup(email="gmail2@gmail.com")
        token_1 = res_1.json()['token']
        token_2 = res_2.json()['token']

        self._create_group(token=token_1, name="abc", description= "abcdef")
        self._create_group(token=token_2, name="xyz", description= "xyzzzz")

        res = self._get_groups(token_1)

        self._assertEqual(200, res.status_code)
        self._assertEqual(True, 'groups' in res.json())
        self._assertEqual(1, len(res.json().keys()))
        self._assertEqual("xyz", res.json()['groups'][0]['name'])
        self._assertEqual("xyzzzz", res.json()['groups'][0]['description'])

        self._assertEqual("abc", res.json()['groups'][1]['name'])
        self._assertEqual("abcdef", res.json()['groups'][1]['description'])


    def test_new_user_can_send_join_request_to_a_group(self):
        res_1 = self.ـsignup(email="gmail1@gmail.com")
        token_1 = res_1.json()['token']

        res_2 = self._create_group(token=None, name="abc", description= "abcdef")
        group_id = res_2.json()['group']['id']

        res = self._send_join_request(token_1, group_id)

        self._assertEqual(200, res.status_code)
        self._assertEqual(True, 'message' in res.json())
        self._assertEqual(1, len(res.json().keys()))
        self._assertEqual("successfull", res.json()['message'])


    def test_user_can_see_his_join_requests(self):
        res_1 = self.ـsignup(email="gmail1@gmail.com")
        token_1 = res_1.json()['token']
        jwt_payload_user_1 = self.__extract_jwt_payload(token_1)

        res_1 = self._create_group(name="abc", description= "abcdef")
        group_id_1 = res_1.json()['group']['id']
        res_2 = self._create_group(name="abc", description= "abcdef")
        group_id_2 = res_2.json()['group']['id']

        self._send_join_request(token_1, group_id_1)
        self._send_join_request(token_1, group_id_2)

        response = self._get_my_join_requests(token_1)
        self._assertEqual(200, response.status_code)
        self._assertEqual(True, 'joinRequests' in response.json())
        self._assertEqual(1, len(response.json().keys()))
        self._assertEqual(2, len(response.json()['joinRequests']))
        self._assertEqual(True, self._contains_in_dict(['id', 'userId', 'date', 'groupId'], response.json()['joinRequests'][0]))
        self._assertEqual(True, self._contains_in_dict(['id', 'userId', 'date', 'groupId'], response.json()['joinRequests'][1]))
        self._assertEqual(group_id_2, response.json()['joinRequests'][0]['groupId'])
        self._assertEqual(group_id_1, response.json()['joinRequests'][1]['groupId'])
        # check userId


    def test_admin_can_give_permission(self):
        res_1 = self.ـsignup(email="gmail1@gmail.com")
        token_user_1 = res_1.json()['token']

        res_2 = self.ـsignup(email="gmail2@gmail.com")
        token_user_2 = res_2.json()['token']
        user_2_id = self._get_user_id_from_token(token_user_2)

        self._create_group(token=token_user_1,name="abc", description= "abcdef")

        response = self._give_permission(token_user_1, user_2_id)

        self._assertEqual(200, response.status_code)
        self._assertEqual(True, 'message' in response.json())
        self._assertEqual(1, len(response.json().keys()))
        self._assertEqual(True, "successful" in response.json()['message'])


    def test_admin_can_get_join_requests(self):

        res_1 = self.ـsignup(email="gmail1@gmail.com")
        token_user_1 = res_1.json()['token']
        res_3 = self._create_group(token=token_user_1,name="abc", description= "abcdef")
        group_id = res_3.json()['group']['id']


        res_2 = self.ـsignup(email="gmail22@gmail.com")
        token_user_2 = res_2.json()['token']
        user_2_id = self._get_user_id_from_token(token_user_2)
        self._send_join_request(token_user_2, group_id)

        res_3 = self.ـsignup(email="gmail33@gmail.com")
        token_user_3 = res_3.json()['token']
        user_3_id = self._get_user_id_from_token(token_user_3)
        self._send_join_request(token_user_3, group_id)

        response = self._get_group_join_requests(token=token_user_1)

        self._assertEqual(200, response.status_code)
        self._assertEqual(True, 'joinRequests' in response.json())
        self._assertEqual(1, len(response.json().keys()))
        self._assertEqual(2, len(response.json()['joinRequests']))
        self._assertEqual(True, self._contains_in_dict(['id', 'userId', 'date', 'groupId'], response.json()['joinRequests'][0]))
        self._assertEqual(True, self._contains_in_dict(['id', 'userId', 'date', 'groupId'], response.json()['joinRequests'][1]))
        self._assertEqual(group_id, response.json()['joinRequests'][0]['groupId'])
        self._assertEqual(group_id, response.json()['joinRequests'][1]['groupId'])
        self._assertEqual(user_3_id, response.json()['joinRequests'][0]['userId'])
        self._assertEqual(user_2_id, response.json()['joinRequests'][1]['userId'])


    def test_join_member_and_group_data_works(self):
        # admin register + group creation
        res_1 = self.ـsignup(email="gmail1@gmail.com")
        token_user_1 = res_1.json()['token']
        res_3 = self._create_group(token=token_user_1,name="abc", description= "abcdef")
        group_id = res_3.json()['group']['id']

        # normal user register + send join request
        res_2 = self.ـsignup(email="gmail22@gmail.com")
        token_user_2 = res_2.json()['token']
        user_2_id = self._get_user_id_from_token(token_user_2)
        self._send_join_request(token_user_2, group_id)



        # check admin approve works
        res_3 = self._get_group_join_requests(token=token_user_1)
        join_requests = res_3.json()['joinRequests']
        join_request_id = join_requests[0]['id']
        response_1 = self._acccept_join_request(token_user_1, join_request_id)
        self._assertEqual(200, response_1.status_code)
        self._assertEqual(True, 'message' in response_1.json())
        self._assertEqual(1, len(response_1.json().keys()))
        self._assertEqual(True, "successful" in response_1.json()['message'])

        # check group info
        response_2 = self._get_my_group(token_user_1)
        self._assertEqual(200, response_2.status_code)
        self._assertEqual(True, 'group' in response_2.json())
        self._assertEqual(1, len(response_2.json().keys()))
        self._assertEqual(True, self._contains_in_dict(['name', 'description', 'members'], response_2.json()['group']))
        # self._assertEqual(True, self.)



    # def test_user_can_see_his_group_info(self):
    #     res_1 = self.ـsignup(email="gmail1@gmail.com")
    #     res_2 = self.ـsignup(email="gmail2@gmail.com")
    #     token_1 = res_1.json()['token']
    #     token_2 = res_2.json()['token']

    #     self._create_group(token_1, name="abc", description= "abcdef")
    #     self._create_group(token_2, name="xyz", description= "xyzzzz")

    #     group_1 = self._get_own_group(token_1)
    #     group_2 = self._get_own_group(token_2)

tests = [
    ('is_system_up', 1),

    ('test_sign_up_work_with_proper_data', 1),
    ('test_user_id_is_in_jwt_token_of_signup', 1),
    ('test_email_is_in_jwt_token_of_signup', 1),

    ('test_login_works_with_proper_data', 1),
    ('test_user_id_is_in_jwt_token_of_login', 1),
    ('test_email_is_in_jwt_token_of_login', 1),

    ('test_create_group_works_for_new_user', 3),
    ('test_can_get_groups_after_successfully_created', 5),
    ('test_new_user_can_send_join_request_to_a_group', 5),
    ('test_user_can_see_his_join_requests', 3),
    ('test_admin_can_get_join_requests', 3),
    ('test_join_member_and_group_data_works', 5)
]


if __name__ == "__main__":
    host = sys.argv[1]
    judge = Judge(host)

    total = 0
    for i, (test, score) in enumerate(tests, 1):
        reset_db()
        test_name = ' '.join(test.split('_')).title()
        try:
            getattr(judge, test)()
            total = total + score
            print(f'{i}. {test_name}: PASSED')
        except Exception as e:
            print(f'{i}. {test_name}: FAILED')
            raise e

    print(f'\nTotal score: {total}/{sum(score for test, score in tests)}')